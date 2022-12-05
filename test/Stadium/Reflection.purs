module Test.Stadium.Reflection where

import Prelude

import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Stadium as SD
import Stadium.Reflection as SDR
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec = pure unit
  -- do
  -- describe "StatePath" do
  --   describe "Leaf" do
  --     it "reflects to a data type" do
  --       SDR.reflectStatePath (Proxy :: _ (SD.StatePath_Leaf)) `shouldEqual` SDR.StatePath_Leaf
  --   describe "Cases" do
  --     it "reflects to a data type" do
  --       SDR.reflectStatePath (Proxy :: _ (SD.StatePath_Cases ())) `shouldEqual` (SDR.StatePath_Cases Map.empty)
  --   describe "Cases" do
  --     it "reflects to a data type" do
  --       SDR.reflectStatePath
  --         ( Proxy
  --             :: _
  --                  ( SD.StatePath_Cases
  --                      ( foo :: SD.StatePath_Leaf
  --                      )
  --                  )
  --         ) `shouldEqual`
  --         ( SDR.StatePath_Cases $ Map.fromFoldable
  --             [  "foo" /\ SDR.StatePath_Leaf
  --             ]
  --         )