{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}
module Players
  ( Players
  , empty
  , PlayerId
  , Self(..)
  , getSelf
  , markSelfAsDead
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

advance :: Turns -> Turns
advance NoTurns = NoTurns
advance Turns{past, present, future=[]} =
  let x:xs = reverse (present : past)
    in Turns
      { past = []
      , present = x
      , future = xs
      }

advance Turns{past, present, future=x:xs} =
  Turns
    { past = present : past
    , present = x
    , future = xs
    }

insert :: PlayerId -> Turns -> Turns
insert pid NoTurns = Turns
  { past = []
  , present = pid
  , future = []
  }
insert pid Turns{past, present, future} =
  Turns{past, present, future = future ++ [pid]}

remove :: PlayerId -> Turns -> Turns
remove _pid NoTurns = NoTurns
remove pid Turns{past, present, future}
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

markSelfAsDead :: Lens' st (Players p) -> Game.GameM st eff env Connection ()
markSelfAsDead players = do
  connection <- getConnection
  Self pid _ <- getSelf players

  players %=
    (connections %~ Bimap.delete connection)
    . (turns %~ remove pid)
