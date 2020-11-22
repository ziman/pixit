module Api where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)

import Game.WSGame.Engine (HasError(..))

data Player = Player
  { name :: Text
  , score :: Int
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

data State = State
  { players :: [Player]
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

data Message_S2C
  = Error { message :: String }
  | Update { state :: State }
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance HasError Message_S2C where
  s2cError = Error

data Message_C2S
  = Join { playerName :: Text }
  deriving (Eq, Ord, Show, Generic, FromJSON)
