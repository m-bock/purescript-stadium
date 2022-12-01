{ name = "stadium"
, dependencies =
  [ "console"
  , "effect"
  , "prelude"
  , "tuples"
  , "typelevel-prelude"
  , "variant"
  , "variant-ctors"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
