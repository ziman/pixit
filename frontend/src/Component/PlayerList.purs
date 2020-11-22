module Component.PlayerList (new) where

import Prelude

import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R

import Api as Api

type Props = { players :: Array Api.Player }
type State = Unit

render :: Self Props State -> JSX
render self =
  R.table
  { className: "player-list"
  , children:
    [ R.tbody
      { children: do
          player <- self.props.players
          pure $ R.tr
            { className: "alive"
            , children:
              [ R.td
                { className: "player-name"
                , children: [R.text player.name]
                }
              , R.td
                { className: "player-score"
                , children:
                  [ R.text $ show player.score
                  ]
                }
              ]
            }
      }
    ]
  }

new :: Props -> JSX
new = make (createComponent "UserList")
  { initialState: unit
  , render
  }

-- vim: et ts=2 sts=2 sw=2
