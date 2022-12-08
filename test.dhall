let main = ./spago.dhall

in main //
  { dependencies = main.dependencies #
    [ "aff"
    , "debug"
    , "spec"
    , "unsafe-reference"
    ]
  , sources = main.sources # [ "test/**/*.purs" ]
  }