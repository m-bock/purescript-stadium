module Stadium.IsoVariant where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Data.Variant as V
import Type.Proxy (Proxy(..))

class IsoVariant v a | a -> v where
  fromVariant :: Variant v -> a
  toVariant :: a -> Variant v

instance IsoVariant (true :: Unit, false :: Unit) Boolean where
  toVariant true = V.inj (Proxy :: _ "true") unit
  toVariant false = V.inj (Proxy :: _ "false") unit

  fromVariant = V.case_ # V.onMatch
    { "true": \_ -> true
    , "false": \_ -> false
    }

instance IsoVariant ("Just" :: a, "Nothing" :: Unit) (Maybe a) where
  toVariant Nothing = V.inj (Proxy :: _ "Nothing") unit
  toVariant (Just x) = V.inj (Proxy :: _ "Just") x

  fromVariant = V.case_ # V.onMatch
    { "Nothing": \_ -> Nothing
    , "Just": Just
    }
