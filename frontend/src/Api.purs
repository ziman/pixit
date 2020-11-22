module Api where

import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either(..))
import Data.Set (Set)
import Data.Tuple (Tuple)
import Foreign.Object as Object
import Data.Argonaut.Core (caseJsonObject, fromString, fromObject, Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Argonaut.Decode.Combinators ((.:))

type Player =
  { name :: String
  , score :: Int
  }

type State =
  { players :: Array Player
  }

data Message_S2C
  = Error { message :: String }
  | Update { state :: State }

instance msg_s2c_DecodeJson :: DecodeJson Message_S2C where
  decodeJson json = do
    obj <- decodeJson json
    obj .: "tag" >>= case _ of
      "Update" -> Update <$> decodeJson json
      "Error" -> Error <$> decodeJson json
      tag -> Left $ AtKey "tag" $ UnexpectedValue (fromString tag)

data Message_C2S
  = Join { playerName :: String }

instance msg_c2s_EncodeJson :: EncodeJson Message_C2S where
  encodeJson = case _ of
    Join obj -> "Join" // obj

infix 3 addTag as //
addTag :: forall a. EncodeJson a => String -> a -> Json
addTag t obj = caseJsonObject json f json
  where
    json = encodeJson obj
    f obj' = fromObject $ Object.insert "tag" (fromString t) obj'
