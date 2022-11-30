module Stadium
  ( Case
  , Case_Leaf
  , Case_StatePath
  , Field
  , Field_Cases
  , Field_Fields
  , Protocol
  , Protocol_
  , StatePath
  , StatePath_Cases
  , StatePath_Fields
  , Transition
  , Transition_
  , mkReducer
  )
  where

import Unsafe.Coerce (unsafeCoerce)

--- Protocol

foreign import data Protocol :: Type

foreign import data Protocol_ :: Row Transition -> Protocol

--- StatePath

foreign import data StatePath :: Type

foreign import data StatePath_Fields :: Row Field -> StatePath

foreign import data StatePath_Cases :: Row Case -> StatePath

--- Field

foreign import data Field :: Type

foreign import data Field_Fields :: Row Field -> Field

foreign import data Field_Cases :: Row Case -> Field

--- Case
foreign import data Case :: Type

foreign import data Case_Leaf :: Case

foreign import data Case_StatePath :: StatePath -> Case

--- Transition

foreign import data Transition :: Type

foreign import data Transition_ :: StatePath -> StatePath -> Transition

--- MkReducer

mkReducer :: forall a. a
mkReducer = unsafeCoerce 1