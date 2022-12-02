{ name = "stadium"
, dependencies =
  [ "aff"
  , "console"
  , "control"
  , "effect"
  , "heterogeneous"
  , "identity"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "record"
  , "spec"
  , "tuples"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
