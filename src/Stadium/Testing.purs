module Stadium.Testing
  ( (>>?)
  , MsgToState(..)
  , testReducer
  , type (>>?)
  ) where

import Prelude

import Data.Traversable (scanl)

testReducer
  :: forall msg sta bool
   . (Array sta -> Array sta -> bool)
  -> ((msg -> sta -> sta) -> sta -> Array (msg >>? sta) -> bool)
testReducer eq reducer init spec =
  scanl (flip reducer) init (msg <$> spec) `eq` (sta <$> spec)

data MsgToState msg sta = MsgToState msg sta

msg :: forall msg sta. MsgToState msg sta -> msg
msg (MsgToState msg sta) = msg

sta :: forall msg sta. MsgToState msg sta -> sta
sta (MsgToState msg sta) = sta

infixr 6 MsgToState as >>?

infixr 6 type MsgToState as >>?
