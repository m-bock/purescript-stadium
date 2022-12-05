module Test.TestReadme where

import Prelude

import Data.Variant as V
import Stadium.Testing (testReducer, (>>?))
import Test.Readme as RM
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Type.Proxy (Proxy(..))

spec_state'1 :: Spec Unit
spec_state'1 = do
  describe "reducer'1" do
    it "performs state transitions correctly" do
      testReducer shouldEqual RM.reducer'1
        RM.On
        [ RM.SwitchOn >>? RM.On
        , RM.SwitchOff >>? RM.Off
        , RM.SwitchOff >>? RM.Off
        , RM.SwitchOn >>? RM.On
        ]

spec_state'2 :: Spec Unit
spec_state'2 = do

  describe "reducer'2" do
    it "performs state transitions correctly" do
      testReducer shouldEqual RM.reducer'2
        _off
        [ _switchOn >>? _on
        , _switchOff >>? _off
        , _switchOff >>? _off
        , _switchOn >>? _on
        ]
  where
  _off = V.inj (Proxy :: _ "off") unit
  _on = V.inj (Proxy :: _ "on") unit
  _switchOn = V.inj (Proxy :: _ "switchOn") unit
  _switchOff = V.inj (Proxy :: _ "switchOff") unit

spec :: Spec Unit
spec = do
  spec_state'1
  spec_state'2
