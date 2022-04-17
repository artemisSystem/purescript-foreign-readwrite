{ name = "foreign-readwrite"
, dependencies =
  [ "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "identity"
  , "lists"
  , "maybe"
  , "prelude"
  , "record"
  , "safe-coerce"
  , "transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/artemisSystem/purescript-foreign-readwrite"
}
