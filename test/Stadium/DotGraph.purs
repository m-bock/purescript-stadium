module Test.Stadium.DotGraph where

import Prelude

import Data.DotLang.Class as Dot
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Stadium as SD
import Stadium.DotGraph as DG
import Stadium.Reflection as SDR
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

spec = pure unit

-- spec :: Spec Unit
-- spec = do
--   describe "StatePath" do
--     describe "Leaf" do
--       it "reflects to a data type" do
--         txt <- readTextFile UTF8 "test/golden/one.dot"
--         (DG.toDotGraph (SDR.Protocol_ Map.empty) # Dot.toText) `shouldEqual` txt

