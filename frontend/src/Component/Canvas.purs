module Component.Canvas (new, onDraw, onUpdateBitmap) where

import Prelude
import Effect (Effect)
import Effect.Exception (throw)

import Data.Maybe (Maybe(..))

import React.Basic (JSX)
import React.Basic.Events (EventHandler)
import React.Basic.DOM.Events (capture_)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R

import Graphics.Canvas
  ( canvasToDataURL, getContext2D, moveTo, lineTo, stroke
  , setLineWidth, setStrokeStyle, getCanvasElementById
  , CanvasElement, setLineCap, setLineJoin, beginPath
  , LineCap(..), LineJoin(..)
  , tryLoadImage, drawImage
  )

import Api as Api
import Utils (canvasHandler, CanvasEvent)

type Props =
  { onDraw :: Api.Segment -> Effect Unit
  , onUpdateBitmap :: Api.Base64Png -> Effect Unit
  }

type State =
  { lineStart :: Maybe Api.Point
  , thickness :: Number
  , colour :: Api.Colour
  }

onDraw :: Api.Segment -> Effect Unit
onDraw segment =
  getCanvasElementById "canvas" >>= case _ of
    Nothing -> throw "could not find canvas"
    Just canvas -> drawSegment canvas segment

onUpdateBitmap :: Api.Base64Png -> Effect Unit
onUpdateBitmap dataUrl =
  tryLoadImage dataUrl case _ of
    Nothing -> pure unit  -- not loadable
    Just img -> getCanvasElementById "canvas" >>= case _ of
      Nothing -> throw "could not find canvas"
      Just canvas -> do
        g <- getContext2D canvas
        drawImage g img 0.0 0.0

drawSegment :: CanvasElement -> Api.Segment -> Effect Unit
drawSegment canvas segment = do
  g <- getContext2D canvas
  setStrokeStyle g segment.colour
  setLineWidth g segment.thickness
  setLineCap g Round
  setLineJoin g RoundJoin
  moveTo g segment.src.x segment.src.y
  lineTo g segment.dst.x segment.dst.y
  stroke g

drawSegmentWrapper :: Self Props State -> Api.Point -> CanvasEvent -> Effect Unit
drawSegmentWrapper self src evt = do
    drawSegment evt.canvas segment
    self.props.onDraw segment
  where
    segment :: Api.Segment
    segment =
      { src
      , dst: {x: evt.x, y: evt.y}
      , colour: self.state.colour
      , thickness: self.state.thickness
      }

onDown :: Self Props State -> EventHandler
onDown self = canvasHandler \evt -> do
  g <- getContext2D evt.canvas
  beginPath g

  self.setState _
    { lineStart = Just {x: evt.x, y: evt.y}
    }

onUp :: Self Props State -> EventHandler
onUp self = canvasHandler \evt ->
  case self.state.lineStart of
    Nothing -> pure unit
    Just _ -> do
      self.setState _{lineStart = Nothing}
      self.props.onUpdateBitmap =<< canvasToDataURL evt.canvas
      -- TODO:
      -- make a background timer job
      -- that checks self.state to see if there's been an update
      -- if so, send bitmap, clear dirty flag
      --
      -- doing this synchronously warps the ends of curves

onLeave :: Self Props State -> EventHandler
onLeave self = canvasHandler \evt ->
  case self.state.lineStart of
    Nothing -> pure unit
    Just src -> do
      drawSegmentWrapper self src evt

      self.setState _{lineStart = Nothing}
      self.props.onUpdateBitmap =<< canvasToDataURL evt.canvas

onMove :: Self Props State -> EventHandler
onMove self = canvasHandler \evt ->
  case self.state.lineStart of
    Nothing -> pure unit
    Just src -> do
      drawSegmentWrapper self src evt

      self.setState _
        { lineStart = Just {x: evt.x, y: evt.y}
        }

tool :: Self Props State -> R.CSS -> (State -> Boolean) -> (State -> State) -> JSX
tool self css isActive onClick =
  R.li
  { className:
      if isActive self.state
        then "active"
        else "inactive"
  , onClick: capture_ (self.setState onClick)
  , style: css
  }

swatch :: Self Props State -> Api.Colour -> JSX
swatch self col =
  tool
    self
    (R.css {"background-color": col})
    (\st -> st.colour == col)
    _{colour = col}

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
      , id: "canvas"
      }
    , R.ul
      { className: "toolbox"
      , children:
        [ swatch self "#000000"
        , swatch self "#FF0000"
        ]
      }
    ]
  }

new :: Props -> JSX
new = make (createComponent "Placeholder")
  { initialState:
    { lineStart: Nothing
    , thickness: 3.0
    , colour: "#000000"
    }
  , render
  }

-- vim: et ts=2 sts=2 sw=2
