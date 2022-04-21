let main = ./spago.dhall

in main //
  { dependencies = main.dependencies #
    [ "spec"
    ]
  , sources = main.sources # [ "test/**/*.purs" ]
  }