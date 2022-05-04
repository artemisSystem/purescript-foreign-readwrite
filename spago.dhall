{ name = "foreign-readwrite"
, dependencies =
  [ "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "identity"
  , "lists"
  , "maybe"
  , "newtype"
  , "prelude"
  , "record"
  , "safe-coerce"
  , "transformers"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/artemisSystem/purescript-foreign-readwrite"
}
