module Component.Canvas (new) where

import Prelude
import React.Basic (JSX)
import React.Basic.Classic (Self, createComponent, make)
import React.Basic.DOM as R

type Props = Unit
type State = Unit  -- all state is implicit in <canvas>

render :: Self Props State -> JSX
render self =
  R.div
  { className: "canvas"
  , children:
    [ R.canvas
      { width: "800"
      , height: "600"
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
  { initialState: unit
  , render
  }

-- vim: et ts=2 sts=2 sw=2
