module Component.Canvas (new) where

import Prelude
import Effect (Effect)

import Data.Int (round)
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..))

import React.Basic (JSX)
import React.Basic.Events (EventHandler)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R

import Graphics.Canvas (canvasToDataURL, getContext2D, moveTo, lineTo, stroke)

import Api as Api
import Utils (canvasHandler, CanvasEvent)

type Props =
  { onDraw :: Api.Segment -> Effect Unit
  , onUpdateBitmap :: Api.Base64Png -> Effect Unit
  }

type State =
  { lineStart :: Maybe (Tuple Number Number)
  }

black :: Api.Colour
black = {r: 0, g: 0, b: 0}

drawLine :: Self Props State -> Tuple Number Number -> CanvasEvent -> Effect Unit
drawLine self (Tuple srcX srcY) evt = do
  g <- getContext2D evt.canvas
  moveTo g srcX srcY
  lineTo g evt.x evt.y
  stroke g

  self.props.onDraw
    { src: Tuple (round srcX) (round srcY)
    , dst: Tuple (round evt.y) (round evt.y)
    , colour: black
    , thickness: 1
    }

onDown :: Self Props State -> EventHandler
onDown self = canvasHandler \evt ->
  self.setState _
    { lineStart = Just (Tuple evt.x evt.y)
    }

onUp :: Self Props State -> EventHandler
onUp self = canvasHandler \evt ->
  case self.state.lineStart of
    Nothing -> pure unit
    Just _ -> do
      self.setState _{lineStart = Nothing}
      self.props.onUpdateBitmap =<< canvasToDataURL evt.canvas

onLeave :: Self Props State -> EventHandler
onLeave self = canvasHandler \evt ->
  case self.state.lineStart of
    Nothing -> pure unit
    Just src -> do
      drawLine self src evt

      self.setState _{lineStart = Nothing}
      self.props.onUpdateBitmap =<< canvasToDataURL evt.canvas

onMove :: Self Props State -> EventHandler
onMove self = canvasHandler \evt ->
  case self.state.lineStart of
    Nothing -> pure unit
    Just src -> do
      drawLine self src evt

      self.setState _
        { lineStart = Just (Tuple evt.x evt.y)
        }


render :: Self Props State -> JSX
render self =
  R.div
  { className: "canvas"
  , children:
    [ R.canvas
      { width: "800"
      , height: "600"
      , onMouseDown: onDown self
      , onMouseUp: onUp self
      , onMouseLeave: onLeave self
      , onMouseMove: onMove self
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
