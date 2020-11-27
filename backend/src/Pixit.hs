{-# LANGUAGE TemplateHaskell #-}
module Pixit (game, mkInitialState) where

import Prelude hiding (log, Word, length)
import System.Random

import Data.Function
import Data.Foldable hiding (length)
import Data.Text (Text)
import Data.Map.Strict (Map)
import Data.Bimap (Bimap)
import Data.Sequence (Seq)
import qualified Data.Bimap as Bimap
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vec
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified VectorShuffling.Immutable as Vec

import Control.Monad (when)
import Control.Monad.State.Class

import Game.WSGame.Engine (Connection)
import Game.WSGame.Game as Game
import qualified Game.WSGame.Engine as Engine

import Lens.Micro.Platform

import qualified Api

data Player = Player
  { _name :: Text
  , _score :: Int
  }

makeLenses ''Player

instance Show Player where
  show Player{_name, _score}
    = show (_name, _score)

newtype PlayerId = PlayerId {unPlayerId :: Int}
  deriving newtype (Eq, Ord, Show)

data State = State
  { _players :: Map PlayerId Player
  , _nextPlayerId :: PlayerId
  , _connections :: Bimap Connection PlayerId
  , _chatMessages :: Seq Api.ChatMessage
  -- , _wordlist :: Vector Text
  }
  deriving Show

makeLenses ''State

data Effect
  = Send Connection Api.Message_S2C
  | Close Connection
  | Log String

type Pixit a = Game.GameM State Effect () Connection a

log :: String -> Pixit ()
log msg = perform $ Log msg

send :: Connection -> Api.Message_S2C -> Pixit ()
send conn msg = perform $ Send conn msg

close :: Connection -> Pixit ()
close conn = perform $ Close conn

data Self = Self
  { _pid :: PlayerId
  , _self :: Traversal' State Player
  }

getSelf :: Pixit Self
getSelf = do
  state <- getState
  connection <- getConnection
  case Bimap.lookup connection (state ^. connections) of
    Nothing -> throwHard $ "connection not in game state: " ++ show connection
    Just pid -> do
      -- check the PID because `ix` won't do that
      when (not $ has (players . ix pid) state) $
        throwHard $ "no player for PID " ++ show pid

      return $ Self pid (players . ix pid)

onDeadPlayer :: Pixit ()
onDeadPlayer = do
  connection <- getConnection
  connections %= Bimap.delete connection
  broadcastStateUpdate

sendStateUpdate :: Connection -> Player -> State -> Pixit ()
sendStateUpdate conn _player st =
  send conn $ Api.Update $ Api.State
    { players =
      [ Api.Player
        { name    = p ^. name
        , score   = p ^. score
        , isDead  = False  -- TODO
        , isDrawing = False  -- TODO
        }
      | (_pid, p) <- Map.toList (st ^. players)
      ]
    , chatMessages = st ^. chatMessages
    }

broadcastStateUpdate :: Pixit ()
broadcastStateUpdate = do
  st <- getState
  for_ (Bimap.toList $ st ^. connections) $ \(connection, pid) ->
    sendStateUpdate connection ((st ^. players) Map.! pid) st

handle :: Api.Message_C2S -> Pixit ()
handle Api.Join{playerName} = do
  st <- getState
  thisConnection <- getConnection

  -- check if this player already exists
  case [(pid, p) | (pid, p) <- Map.toList (st ^. players), (p ^. name) == playerName] of
    -- existing player
    (pid, player):_ ->
      case [c | (c, pid') <- Bimap.toList (st ^. connections), pid' == pid] of
        -- currently connected
        oldConnection:_ -> do
          log $ show thisConnection ++ " replaces live player "
            ++ show (oldConnection, player ^. name)

          -- replace the player
          connections %=
              Bimap.delete oldConnection
            . Bimap.insert thisConnection pid

          -- close the old connection
          close oldConnection

        -- dead player
        [] -> do
          log $ show thisConnection ++ " resurrects dead player "
            ++ show (player ^. name)

          -- resurrect the player
          connections %= Bimap.insert thisConnection pid

    -- brand new player
    [] -> do
      -- create a new player
      log $ show thisConnection ++ " is a new player"

      modify $ \st -> st
        & (players . at (st ^. nextPlayerId) .~ Just Player
            { _name = playerName
            , _score = 0
            }
          )
        & (connections %~ Bimap.insert thisConnection (st ^. nextPlayerId))
        & (nextPlayerId %~ (PlayerId . (+1) . unPlayerId))

  broadcastStateUpdate

handle Api.Broadcast_C2S{broadcast} = do
  Self selfPid _self <- getSelf  -- TODO: remove this
  connectionsPids <- Bimap.toList <$> use connections
  for_ connectionsPids $ \(connection, pid) ->
    when (pid /= selfPid) $
      send connection Api.Broadcast_S2C{broadcast}

handle Api.SendMessage{text} = do
  Self _selfPid self <- getSelf
  -- TODO: check guess
  selfName <- use $ self . name
  modify $
    chatMessages %~ (Api.Chat{name = selfName, text} Seq.<|)
  broadcastStateUpdate

game :: Engine.Game State Effect () Api.Message_C2S Api.Message_S2C
game = Engine.Game
  { onMessage = handle
  , onDeadPlayer
  , runEffect
  }

mkInitialState :: FilePath -> IO State
mkInitialState fnLanguage = do
  wordlist <- Vec.fromList . Text.words <$> Text.readFile fnLanguage
  putStrLn $ "loaded " ++ show (Vec.length wordlist) ++ " words"
  g <- newStdGen
  let (_wordlistShuffled, _g') = Vec.shuffle wordlist g
  pure $ State
    { _players = Map.empty
    , _connections = Bimap.empty
    , _nextPlayerId = PlayerId 1
    , _chatMessages = Seq.empty
    -- , _wordlist = wordlistShuffled
    }

runEffect :: Effect -> IO ()
runEffect = \case
  Log msg -> putStrLn msg
  Close conn -> Engine.close @Api.Message_S2C conn
  Send conn msg -> Engine.send conn msg
