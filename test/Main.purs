module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Stadium.Reflection as Test.Stadium.Reflection
import Test.Stadium.DotGraph as Test.Stadium.DotGraph


main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  Test.Stadium.Reflection.spec
  Test.Stadium.DotGraph.spec