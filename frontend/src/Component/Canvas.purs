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
import Graphics.Canvas (CanvasElement, canvasToDataURL, getContext2D, moveTo, lineTo, stroke)
import Web.HTML.HTMLElement (getBoundingClientRect, fromEventTarget)

import Api as Api

type Props =
  { onDraw :: Api.Segment -> Effect Unit
  , onUpdateBitmap :: Api.Base64Png -> Effect Unit
  }

type State =
  { lineStart :: Maybe (Tuple Number Number)
  }

black :: Api.Colour
black = {r: 0, g: 0, b: 0}

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
                { lineStart = Just (Tuple cx cy)
                }

            _ -> pure unit

      , onMouseUp: capture target \tgt -> do
          self.setState _
            { lineStart = Nothing
            }

          let canvas = unsafeCoerce tgt :: CanvasElement
          self.props.onUpdateBitmap =<< canvasToDataURL canvas

      , onMouseMove: capture (merge {clientX, clientY, target}) \evt -> do

          case Tuple (Tuple evt.clientX evt.clientY) (fromEventTarget evt.target) of
            Tuple (Tuple (Just cx) (Just cy)) (Just elm) -> do
              case self.state.lineStart of
                Nothing -> pure unit
                Just (Tuple srcX srcY) -> do
                  rect <- getBoundingClientRect elm

                  let canvas = unsafeCoerce evt.target :: CanvasElement
                      dstX = cx - rect.left
                      dstY = cy - rect.top

                  g <- getContext2D canvas
                  moveTo g srcX srcY
                  lineTo g dstX dstY
                  stroke g

                  self.setState _
                    { lineStart = Just (Tuple dstX dstY)
                    }

                  self.props.onDraw
                    { src: Tuple (round srcX) (round srcY)
                    , dst: Tuple (round dstX) (round dstY)
                    , colour: black
                    , thickness: 1
                    }

            _ -> pure unit
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
