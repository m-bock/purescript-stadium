module Test.Stadium where

import Prelude

import Data.Either (Either)
import Data.Variant (Variant)
import Stadium (class ValidProtocol, type (>>), Cases, Protocol, Protocol_, At)
import Type.Proxy (Proxy(..))

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

---

-- type Msg = Either Unit Unit

-- type State = Either String Int

-- test2 :: Unit
-- test2 = isValidProtocol
--   ( Proxy
--       :: _
--            ( Protocol_
--                ( msg1 :: Cases () >> Cases ()
--                , msg2 :: Cases () >> Cases ()
--                )
--            )
--   )
--   ( Proxy
--       :: _
--            Msg
--   )
--   ( Proxy
--       :: _
--            State
--   )
