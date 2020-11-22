module Api where

import GHC.Generics
import Data.Text (Text)
import Data.Word (Word8)
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

data Colour = Colour
  { r :: Word8
  , g :: Word8
  , b :: Word8
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

data Segment = Segment
  { src :: (Int, Int)
  , dst :: (Int, Int)
  , colour :: Colour
  , thickness :: Int
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

newtype Base64Png = Base64Png Text
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSON)

data Message_C2S
  = Join { playerName :: Text }
  | Broadcast_C2S { broadcast :: Broadcast }
  deriving (Eq, Ord, Show, Generic, FromJSON)
