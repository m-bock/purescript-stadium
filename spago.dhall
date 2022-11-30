{ name = "stadium"
, dependencies =
  [ "console", "effect", "prelude", "tuples", "typelevel-prelude", "variant" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
