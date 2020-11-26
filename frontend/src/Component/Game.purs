module Component.Game (new) where

import Prelude

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R

import Web.HTML as HTML
import Web.HTML.Window as Window
import Web.HTML.Location as Location

import WebSocket as WS
import Effect (Effect)
import Effect.Exception (throwException)

import Api as Api
import Utils as Utils
import Component.Login as Login
import Component.PlayerList as PlayerList
import Component.Canvas as Canvas
import Component.Chat as Chat

type WebSocket = WS.Capabilities Effect Api.Message_C2S
type Props = Unit
data State
  = LoggedOut
  | LoggedIn WebSocket Api.State

onMessage :: Self Props State -> WebSocket -> Api.Message_S2C -> Effect Unit
onMessage self sock (Api.Update u) = do
  self.setState \s -> LoggedIn sock u.state

onMessage self sock (Api.Error e) = do
  Utils.alert e.message

onMessage self sock (Api.Broadcast_S2C {broadcast}) =
  case broadcast of
    Api.Draw {segment} -> Canvas.onDraw segment
    Api.UpdateBitmap {bitmap} -> Canvas.onUpdateBitmap bitmap

getWsUrl :: Effect String
getWsUrl = do
  l <- Window.location =<< HTML.window
  p <- Location.protocol l
  h <- Location.host l
  pure $ (if p == "https:" then "wss://" else "ws://") <> h <> "/ws"

render :: Self Props State -> JSX
render self =
  case self.state of
    LoggedOut ->
      R.div
      { className: "game"
      , children:
        [ Login.new
          { onSubmit: \playerName -> do
              Utils.log playerName
              wsUrl <- getWsUrl
              WS.newWebSocket wsUrl [] $
                WS.WebSocketsApp \env ->
                  { onopen: \sock ->
                      sock.send $ Api.Join {playerName}
                  , onmessage: \sock msg ->
                      onMessage self sock msg
                  , onerror: \err -> do
                      Utils.alert "connection error"
                      self.setState \s -> LoggedOut
                      throwException err
                  , onclose: \evt -> do
                      self.setState \s -> LoggedOut
                  }
          }
        ]
      }

    LoggedIn sock state ->
      R.div
      { className: "game"
      , children:
        [ R.div
          { className: "left-column"
          , children:
            [ PlayerList.new {players: state.players}
            ]
          }
        , R.div
          { className: "main"
          , children:
            [ Canvas.new
              { onDraw: \segment -> do
                  sock.send $ Api.Broadcast_C2S
                    { broadcast: Api.Draw {segment}
                    }
              , onUpdateBitmap: \bitmap -> do
                  sock.send $ Api.Broadcast_C2S
                    { broadcast: Api.UpdateBitmap {bitmap}
                    }
              }
            ]
          }
        , R.div
          { className: "right-column"
          , children:
            [ Chat.new
              { messages: state.chatMessages
              , onSend: \text ->
                  sock.send $ Api.SendMessage {text}
              }
            ]
          }
        ]
      }

new :: Props -> JSX
new = make (createComponent "Game")
  { initialState: LoggedOut
  , render
  }

-- vim: et ts=2 sts=2 sw=2
