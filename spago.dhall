{ name = "stadium"
, dependencies =
  [ "aff", "data-slices", "effect", "prelude", "spec", "variant" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
