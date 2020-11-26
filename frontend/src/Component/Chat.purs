module Component.Chat (new) where

import Prelude
import Data.Maybe (Maybe(..))
import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R

import Api as Api

type Props = Unit
type State =
  { messages :: Array Api.ChatMessage
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
        [ R.text case msg.text of
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
    [ R.ul
      { children: map renderMessage self.state.messages
      }
    , R.input
      { "type": "text"
      }
    ]
  }

new :: Props -> JSX
new = make (createComponent "Chat")
  { initialState:
    { messages:
      [ Api.Chat
        { name: "Pixit"
        , text: "Welcome to Pixit!"
        }
      , Api.CorrectGuess
        { name: "Joe"
        , text: Nothing
        }
      , Api.Chat
        { name: "Pixit"
        , text: "Welcome to Pixit!"
        }
      , Api.CorrectGuess
        { name: "Joe"
        , text: Just "boo"
        }
      ]
    }
  , render
  }

-- vim: et ts=2 sts=2 sw=2
