{ name = "scrabble"
, dependencies =
  [ "console"
  , "effect"
  , "aff"
  , "psci-support"
  , "argonaut"
  , "argonaut-codecs"
  , "websocket-moderate"
  , "react-basic"
  , "react-basic-classic"
  , "react-basic-dom"
  , "media-types"
  , "unsafe-coerce"
  , "canvas"
  , "integers"
  , "css"
  , "colors"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
