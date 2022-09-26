let main = ./spago.dhall

in main //
  { dependencies = main.dependencies #
    [ "aff"
    , "spec"
    , "unsafe-reference"
    ]
  , sources = main.sources # [ "test/**/*.purs" ]
  }