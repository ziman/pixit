module Api where

import Prelude
import Data.Either (Either(..))
import Data.Tuple (Tuple)
import Foreign.Object as Object
import Data.Argonaut.Core (caseJsonObject, fromString, fromObject, Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Decode.Combinators ((.:))

type Colour =
  { r :: Int
  , g :: Int
  , b :: Int
  }

type Point = Tuple Int Int

type Base64Png = String

type Segment =
  { src :: Point
  , dst :: Point
  , colour :: Colour
  , thickness :: Int
  }

type Player =
  { name :: String
  , score :: Int
  }

type State =
  { players :: Array Player
  }

data Broadcast
  = Draw { segment :: Segment }
  | UpdateBitmap { bitmap :: Base64Png }

instance broadcast_DecodeJson :: DecodeJson Broadcast where
  decodeJson json = do
    obj <- decodeJson json
    obj .: "tag" >>= case _ of
      "Draw" -> Draw <$> decodeJson json
      "UpdateBitmap" -> UpdateBitmap <$> decodeJson json
      tag -> Left $ AtKey "tag" $ UnexpectedValue (fromString tag)

instance broadcast_EncodeJson :: EncodeJson Broadcast where
  encodeJson = case _ of
    Draw obj -> "Draw" // obj
    UpdateBitmap obj -> "UpdateBitmap" // obj

data Message_S2C
  = Error { message :: String }
  | Update { state :: State }
  | Broadcast_S2C { broadcast :: Broadcast }

instance msg_s2c_DecodeJson :: DecodeJson Message_S2C where
  decodeJson json = do
    obj <- decodeJson json
    obj .: "tag" >>= case _ of
      "Update" -> Update <$> decodeJson json
      "Error" -> Error <$> decodeJson json
      "Broadcast_S2C" -> Broadcast_S2C <$> decodeJson json
      tag -> Left $ AtKey "tag" $ UnexpectedValue (fromString tag)

data Message_C2S
  = Join { playerName :: String }
  | Broadcast_C2S { broadcast :: Broadcast }

instance msg_c2s_EncodeJson :: EncodeJson Message_C2S where
  encodeJson = case _ of
    Join obj -> "Join" // obj
    Broadcast_C2S obj -> "Broadcast_C2S" // obj

infix 3 addTag as //
addTag :: forall a. EncodeJson a => String -> a -> Json
addTag t obj = caseJsonObject json f json
  where
    json = encodeJson obj
    f obj' = fromObject $ Object.insert "tag" (fromString t) obj'

-- vim: sw=2 ts=2 sts=2 et
