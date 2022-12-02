{ name = "stadium"
, dependencies =
  [ "aff"
  , "console"
  , "control"
  , "dotlang"
  , "effect"
  , "foldable-traversable"
  , "heterogeneous"
  , "identity"
  , "maybe"
  , "node-buffer"
  , "node-fs-aff"
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
