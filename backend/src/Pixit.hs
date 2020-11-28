{-# LANGUAGE TemplateHaskell #-}
module Pixit (game, mkInitialState) where

import Prelude hiding (log, Word, length)
import System.Random

import Data.Foldable hiding (length)
import Data.Text (Text)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
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

import Players hiding (getSelf)
import qualified Api
import qualified Players

data Profile = Profile
  { _name :: Text
  , _score :: Int
  }

makeLenses ''Profile

instance Show Profile where
  show Profile{_name, _score}
    = show (_name, _score)

data State = State
  { _players :: Players Profile
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

getSelf :: Pixit (Self State Profile)
getSelf = Players.getSelf players

onDeadPlayer :: Pixit ()
onDeadPlayer = do
  Self pid _ <- getSelf
  players %= suspend pid
  broadcastStateUpdate

sendStateUpdate :: Connection -> Profile -> State -> Pixit ()
sendStateUpdate conn _player st =
  send conn $ Api.Update $ Api.State
    { players =
      [ Api.Player
        { name    = p ^. name
        , score   = p ^. score
        , isDrawing = False  -- TODO
        , isSuspended = False  -- TODO
        }
      | (_pid, p) <- st ^.. players . pidsProfiles
      ]
    , chatMessages = st ^. chatMessages
    }

broadcastStateUpdate :: Pixit ()
broadcastStateUpdate = do
  st <- getState
  for_ (st ^.. players . connsPidsProfiles) $ \(connection, _pid, profile) ->
    sendStateUpdate connection profile st

handle :: Api.Message_C2S -> Pixit ()
handle Api.Join{playerName} = do
  st <- getState
  thisConnection <- getConnection

  -- check if this player already exists
  case st ^.. players . pidsProfiles . filtered (\pp -> pp ^. _2 . name == playerName) of
    -- existing player
    (pid, player):_ ->
      case st ^. players . connectionForPid pid of
        -- currently connected
        Just oldConnection -> do
          log $ show thisConnection ++ " replaces live player "
            ++ show (oldConnection, player ^. name)

          -- replace the player
          players %= takeover pid thisConnection

          -- close the old connection
          close oldConnection

        -- suspended player
        Nothing -> do
          log $ show thisConnection ++ " resurrects dead player "
            ++ show (player ^. name)

          -- resurrect the player
          players %= takeover pid thisConnection

    -- brand new player
    [] -> do
      -- create a new player
      log $ show thisConnection ++ " is a new player"

      players %= insert thisConnection Profile{_name = playerName, _score = 0}

  broadcastStateUpdate

handle Api.Broadcast_C2S{broadcast} = do
  Self selfPid _self <- getSelf  -- TODO: remove this
  cps <- getState <&> \st -> st ^.. players . connectionsPids
  for_ cps $ \(connection, pid) ->
    when (pid /= selfPid) $
      send connection Api.Broadcast_S2C{broadcast}

handle Api.SendMessage{text} = do
  Self _selfPid self <- getSelf
  -- TODO: check guess
  selfName <- use $ self . name
  modify $
    chatMessages %~
      (Seq.take 128 . (Api.Chat{name = selfName, text} Seq.<|))
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
    { _players = Players.empty
    , _chatMessages = Seq.empty
    -- , _wordlist = wordlistShuffled
    }

runEffect :: Effect -> IO ()
runEffect = \case
  Log msg -> putStrLn msg
  Close conn -> Engine.close @Api.Message_S2C conn
  Send conn msg -> Engine.send conn msg
