module Api where

import GHC.Generics
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import Data.Sequence (Seq)

import Game.WSGame.Engine (HasError(..))

data Player = Player
  { name :: Text
  , score :: Int
  , isDrawing :: Bool
  , isSuspended :: Bool
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

data ChatMessage
  = Chat
    { name :: Text
    , text :: Text
    }
  | CorrectGuess
    { name :: Text
    , mbText :: Text
    }
  deriving (Eq, Ord, Show, Generic, ToJSON)

data State = State
  { players :: [Player]
  , chatMessages :: Seq ChatMessage
  }
  deriving (Eq, Ord, Show, Generic, ToJSON)

data Broadcast
  = Draw { segment :: Segment }
  | UpdateBitmap { bitmap :: Base64Png }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data Message_S2C
  = Error { message :: String }
  | Update { state :: State }
  | Broadcast_S2C { broadcast :: Broadcast }
  deriving (Eq, Ord, Show, Generic, ToJSON)

instance HasError Message_S2C where
  s2cError = Error

-- CSS colour
newtype Colour = Colour Text
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSON)

data Point = Point
  { x :: Double
  , y :: Double
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data Segment = Segment
  { src :: Point
  , dst :: Point
  , colour :: Colour
  , thickness :: Double
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

newtype Base64Png = Base64Png Text
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSON)

data Message_C2S
  = Join { playerName :: Text }
  | Broadcast_C2S { broadcast :: Broadcast }
  | SendMessage { text :: Text }
  deriving (Eq, Ord, Show, Generic, FromJSON)
