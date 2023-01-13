module Stadium.Reflection where

import Prelude

import Data.Tuple (Tuple)
import DataSlice.Reflection (DataSlice)

newtype Protocol = Protocol_ (Array (Tuple String Transition))

data Transition = Transition_ DataSlice DataSlice

infixr 6 Transition_ as >>
