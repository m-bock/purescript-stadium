module Stadium where

import Prelude

import Data.Reflectable (class Reflectable, reflectType)
import DataSlice (DataSlice)
import DataSlice as DS
import DataSlice.Reflection as DR
import Reflection.Row (class ReflectRow, reflectRow)
import Stadium.Reflection as R
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

--- Protocol

foreign import data Protocol :: Type

foreign import data Protocol_ :: Row Transition -> Protocol

--- Transition

foreign import data Transition :: Type

foreign import data Transition_ :: DataSlice -> DataSlice -> Transition

--- Reflection

instance
  ( Reflectable dsa DR.DataSlice
  , Reflectable dsb DR.DataSlice
  ) =>
  Reflectable (Transition_ dsa dsb) R.Transition where
  reflectType _ = R.Transition_
    (reflectType (Proxy :: _ dsa))
    (reflectType (Proxy :: _ dsb))

instance
  ( ReflectRow trans R.Transition
  ) =>
  Reflectable (Protocol_ trans) R.Protocol where
  reflectType _ = R.Protocol_ $ reflectRow (Proxy :: _ trans)

--- Syntax and Shorthands

infixr 6 type Transition_ as >>
