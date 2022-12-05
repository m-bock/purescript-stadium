let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20221127/packages.dhall
        sha256:9619da55468363705b888350fdd38735a5e90dab101f8d9193057552c5efccad

in  upstream
  with
    dotlang = 
        { dependencies = 
           [ "arrays"
           , "colors"
           , "console"
           , "effect"
           , "maybe"
           , "prelude"
           , "psci-support"
           , "spec"
           , "strings"
           ]
        , repo = "https://github.com/thought2/purescript-dotlang.git"
        , version = "05966e1d33ebd452856ea21a9de943b5e5a330f0"
        }