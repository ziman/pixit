{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
module Players
  ( PlayerId
  , Players
  , empty, insert, takeover, suspend
  , connectionsPids
  , connectionForPid
  , pidsProfiles
  , connsPidsProfiles
  , Self(..), getSelf
  )
  where

import GHC.Stack

import Data.Map (Map)
import Data.Bimap (Bimap)
import qualified Data.Map as Map
import qualified Data.Bimap as Bimap

import Control.Monad (when)

import Game.WSGame.Engine (Connection)
import Game.WSGame.Game as Game

import Lens.Micro.Platform

newtype PlayerId = PlayerId {unPlayerId :: Int}
  deriving newtype (Eq, Ord, Show)

data Turns
  = Turns
      { past :: [PlayerId]    -- reversed past
      , present :: PlayerId
      , future :: [PlayerId]  -- present:future
      }
  | NoTurns
  deriving Show

data Players p = Players
  { _profiles :: Map PlayerId p
  , _turns :: Turns
  , _connections :: Bimap Connection PlayerId
  , _nextPlayerId :: PlayerId
  }
  deriving Show

makeLenses ''Players

connectionsPids :: SimpleFold (Players p) (Connection, PlayerId)
connectionsPids = folding $ Bimap.toList . _connections

pidsProfiles :: SimpleFold (Players p) (PlayerId, p)
pidsProfiles = profiles . folding Map.toList

connsPidsProfiles :: SimpleFold (Players p) (Connection, PlayerId, p)
connsPidsProfiles = folding $ \ps ->
  [ (connection, pid, (ps ^. profiles) Map.! pid)
  | (connection, pid) <- ps ^.. connectionsPids
  ]

connectionForPid :: PlayerId -> SimpleGetter (Players p) (Maybe Connection)
connectionForPid pid = connections . to (Bimap.lookupR pid)

tAdvance :: Turns -> Turns
tAdvance NoTurns = NoTurns
tAdvance Turns{past, present, future=[]} =
  let x:xs = reverse (present : past)
    in Turns
      { past = []
      , present = x
      , future = xs
      }

tAdvance Turns{past, present, future=x:xs} =
  Turns
    { past = present : past
    , present = x
    , future = xs
    }

tInsert :: PlayerId -> Turns -> Turns
tInsert pid NoTurns = Turns
  { past = []
  , present = pid
  , future = []
  }
tInsert pid turns@Turns{past, present, future}
  | pid `elem` past || pid == present || pid `elem` future
  = turns  -- already there

  | otherwise
  = Turns{past, present, future = future ++ [pid]}

tRemove :: PlayerId -> Turns -> Turns
tRemove _pid NoTurns = NoTurns
tRemove pid Turns{past, present, future}
  | present /= pid
  = Turns
    { past = filter (/= pid) past
    , present
    , future = filter (/= pid) future
    }

  -- present == pid

  | x:xs <- future
  = Turns{past, present = x, future = xs}

  -- future == []

  | x:xs <- reverse past
  = Turns{past = [], present = x, future = xs}

  -- past == []

  | otherwise
  = NoTurns

empty :: Players p
empty = Players
  { _profiles = Map.empty
  , _turns = NoTurns
  , _connections = Bimap.empty
  , _nextPlayerId = PlayerId 1
  }

-- take over a potentially suspended player
takeover :: PlayerId -> Connection -> Players p -> Players p
takeover pid connection
  -- tInsert will ignore already existing pids
  = (turns %~ tInsert pid)

  -- Bimap.insert will delete all overlapping pairs
  . (connections %~ Bimap.insert connection pid)

insert :: Connection -> p -> Players p -> Players p
insert connection profile ps =
  ps
    & (profiles . at pid .~ Just profile)
    & (turns %~ tInsert pid)
    & (connections %~ Bimap.insert connection pid)
    & (nextPlayerId %~ (PlayerId . (+1) . unPlayerId))
  where
    pid = ps ^. nextPlayerId

suspend :: PlayerId -> Players p -> Players p
suspend pid
  = (turns %~ tRemove pid)
  . (connections %~ Bimap.deleteR pid)

data Self st p = Self
  { _pid :: PlayerId
  , _profile :: Lens' st p
  }

currentTurn :: SimpleGetter (Players p) (Maybe PlayerId)
currentTurn = to $ \case
  Players{_turns = NoTurns} -> Nothing
  Players{_turns = Turns{present}} -> Just present

unwrap :: HasCallStack => Lens' (Maybe a) a
unwrap = lens (\(Just x) -> x) (const Just)

getSelf :: Lens' st (Players p) -> Game.GameM st eff env Connection (Self st p)
getSelf players = do
  state <- getState
  connection <- getConnection
  case Bimap.lookup connection (state ^. players . connections) of
    Nothing -> throwHard $ "connection not in game state: " ++ show connection
    Just pid -> do
      -- check the PID because `ix` won't do that
      when (not $ has (players . profiles . ix pid) state) $
        throwHard $ "no profile for self PID " ++ show pid

      return Self
        { _pid = pid
        , _profile = players . profiles . at pid . unwrap
        }
