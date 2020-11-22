module Component.Canvas (new) where

import Prelude
import Data.Int (round)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))
import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R
import React.Basic.Events (merge)
import React.Basic.DOM.Events (capture, clientX, clientY, target)
import Unsafe.Coerce (unsafeCoerce)
import Effect (Effect)
import Graphics.Canvas (CanvasElement, canvasToDataURL)

import Api as Api

type Props =
  { onDraw :: Api.Segment -> Effect Unit
  , onUpdateBitmap :: Api.Base64Png -> Effect Unit
  }

type State =
  { lineStart :: Maybe (Tuple Int Int)
  }

render :: Self Props State -> JSX
render self =
  R.div
  { className: "canvas"
  , children:
    [ R.canvas
      { width: "800"
      , height: "600"
      , onMouseDown: capture (merge {clientX,clientY}) \evt ->
          case Tuple evt.clientX evt.clientY of
            Tuple (Just cx) (Just cy) ->
              self.setState _
                { lineStart = Just (Tuple (round cx) (round cy))
                }

            _ -> pure unit

      , onMouseUp: capture target \tgt -> do
          self.setState _
            { lineStart = Nothing
            }

          let canvas = unsafeCoerce tgt :: CanvasElement
          self.props.onUpdateBitmap =<< canvasToDataURL canvas
      }
    , R.div
      { className: "toolbox"
      , children:
        [ R.text "tool1"
        ]
      }
    ]
  }

new :: Props -> JSX
new = make (createComponent "Placeholder")
  { initialState:
    { lineStart: Nothing
    }
  , render
  }

-- vim: et ts=2 sts=2 sw=2
