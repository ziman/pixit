module Api where

import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either(..))
import Foreign.Object as Object
import Data.Argonaut.Core (caseJsonObject, fromString, fromObject, Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Decode.Combinators ((.:))

type Colour = String

type Point =
  { x :: Number
  , y :: Number
  }

type Base64Png = String

type Segment =
  { src :: Point
  , dst :: Point
  , colour :: Colour
  , thickness :: Number
  }

type Player =
  { name :: String
  , score :: Int
  , isDrawing :: Boolean
  , isDead :: Boolean
  }

type State =
  { players :: Array Player
  , chatMessages :: Array ChatMessage
  }

data ChatMessage
  = Chat
    { name :: String
    , text :: String
    }
  | CorrectGuess
    { name :: String
    , text :: Maybe String
    }

instance chatMessage_DecodeJson :: DecodeJson ChatMessage where
  decodeJson json = do
    obj <- decodeJson json
    obj .: "tag" >>= case _ of
      "Chat" -> Chat <$> decodeJson json
      "CorrectGuess" -> CorrectGuess <$> decodeJson json
      tag -> Left $ AtKey "tag" $ UnexpectedValue (fromString tag)

instance chatMessage_EncodeJson :: EncodeJson ChatMessage where
  encodeJson = case _ of
    Chat obj -> "Chat" // obj
    CorrectGuess obj -> "CorrectGuess" // obj

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
  | SendMessage { text :: String }

instance msg_c2s_EncodeJson :: EncodeJson Message_C2S where
  encodeJson = case _ of
    Join obj -> "Join" // obj
    Broadcast_C2S obj -> "Broadcast_C2S" // obj
    SendMessage obj -> "SendMessage" // obj

infix 3 addTag as //
addTag :: forall a. EncodeJson a => String -> a -> Json
addTag t obj = caseJsonObject json f json
  where
    json = encodeJson obj
    f obj' = fromObject $ Object.insert "tag" (fromString t) obj'

-- vim: sw=2 ts=2 sts=2 et
