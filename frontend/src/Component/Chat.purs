module Component.Chat (new) where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R
import React.Basic.Events (handler, merge)
import React.Basic.DOM.Events as RE
import Web.HTML.HTMLInputElement as HtmlInput

import Api as Api

type Props =
  { messages :: Array Api.ChatMessage
  , onSend :: String -> Effect Unit
  }

type State =
  { editLine :: String
  }

renderMessage :: Api.ChatMessage -> JSX
renderMessage (Api.Chat msg) =
  R.li
  { className: "chat"
  , children:
    [ R.span
      { className: "name"
      , children: [R.text msg.name]
      }
    , R.span
      { className: "text"
      , children: [R.text msg.text]
      }
    ]
  }

renderMessage (Api.CorrectGuess msg) =
  R.li
  { className: "correct-guess"
  , children:
    [ R.span
      { className: "name"
      , children: [R.text msg.name]
      }
    , R.span
      { className: "text"
      , children:
        [ R.text case msg.mbText of
            Nothing -> "is correct!"
            Just text -> text
        ]
      }
    ]
  }

render :: Self Props State -> JSX
render self =
  R.div
  { className: "chat"
  , children:
    [ R.input
      { "type": "text"
      , onKeyPress: handler (merge {mbKey: RE.key, target: RE.target}) \evt ->
          if evt.mbKey == Just "Enter"
            then do
              self.props.onSend self.state.editLine
              self.setState _{editLine = ""}

              case HtmlInput.fromEventTarget evt.target of
                Nothing -> pure unit
                Just input -> HtmlInput.setValue "" input

            else pure unit
      , onChange: handler RE.targetValue \mbVal ->
          case mbVal of
            Nothing -> pure unit
            Just val -> self.setState _{editLine = val}
      }
    , R.ul
      { children: map renderMessage self.props.messages
      }
    ]
  }

new :: Props -> JSX
new = make (createComponent "Chat")
  { initialState:
    { editLine: ""
    }
  , render
  }

-- vim: et ts=2 sts=2 sw=2
