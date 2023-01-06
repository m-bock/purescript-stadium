module Test.Stadium where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Variant (Variant)
import Stadium (class ValidProtocol, type (>>), At, Cases, Protocol, Protocol_, Fields)
import Type.Proxy (Proxy(..))
import VariantLike (class GenericVariantLike, class VariantLike, genericFromVariant, genericToVariant)

---
isValidProtocol
  :: forall (ptc :: Protocol) msg sta
   . ValidProtocol ptc msg sta
  => (Proxy ptc -> Proxy msg -> Proxy sta -> Unit)
isValidProtocol _ _ _ = unit

test1 :: Unit
test1 = isValidProtocol
  ( Proxy
      :: _
           ( Protocol_
               ( msg1 :: Cases (b :: At) >> Cases (a :: At)
               , msg2 ::
                   Cases
                     ( a :: Cases (c :: At)
                     )
                     >>
                       Cases (b :: At, a :: Cases (d :: At))
               )
           )
  )
  ( Proxy
      :: _
           ( Variant
               ( msg1 :: String
               , msg2 :: String
               )
           )
  )
  ( Proxy
      :: _
           ( Variant
               ( a :: (Variant (c :: Int, d :: String))
               , b :: String
               )
           )
  )

test2 :: Unit
test2 = isValidProtocol
  ( Proxy
      :: _
           ( Protocol_
               ( msg1 :: Cases () >> Cases ()
               , msg2 :: Cases () >> Cases ()
               )
           )
  )
  ( Proxy
      :: _
           ( Variant
               ( msg1 :: String
               , msg2 :: String
               )
           )
  )
  ( Proxy
      :: _
           ( Variant
               ( a :: Int
               , b :: String
               )
           )
  )

---

data Msg = Msg1 -- | Msg2 | Msg3

derive instance Generic Msg _

instance (GenericVariantLike Msg r) => VariantLike Msg r where
  toVariant = genericToVariant
  fromVariant = genericFromVariant

data State = State1 | State2

type State' = { bla :: Boolean, sub :: State }

derive instance Generic State _

instance (GenericVariantLike State r) => VariantLike State r where
  toVariant = genericToVariant
  fromVariant = genericFromVariant

test3 :: Unit
test3 = isValidProtocol
  ( Proxy
      :: _
           ( Protocol_
               ( "Msg1" ::
                   Fields
                     ( sub :: Cases ("State1" :: At)
                     , bla :: Cases ("true" :: At)
                     )
                     >>
                       Fields
                         ( sub :: Cases ("State1" :: At)
                         )
               )
           )
  )
  ( Proxy
      :: _
           Msg
  )
  ( Proxy
      :: _
           State'
  )
