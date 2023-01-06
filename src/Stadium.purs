module Stadium
  ( At
  , Cases
  , Fields
  , Protocol
  , Protocol_
  , StatePath
  , StatePath_Cases
  , StatePath_Fields
  , StatePath_Leaf
  , Transition
  , Transition_
  , class FilterRow
  , class MatchStatePath
  , class SingleCaseVariant
  , class ValidProtocol
  , class ValidProtocolRL
  , class ValidStatePath
  , class ValidStatePathRL
  , class ValidTransition
  , fromOne
  , oneToOne
  , reSingleCase
  , stateExpand
  , stateGuard
  , toOne
  , type (>>)
  , unSingleCase
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row (class Union)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))

--- Protocol

foreign import data Protocol :: Type

foreign import data Protocol_ :: Row Transition -> Protocol

--- StatePath

foreign import data StatePath :: Type

foreign import data StatePath_Fields :: Row StatePath -> StatePath

foreign import data StatePath_Cases :: Row StatePath -> StatePath

foreign import data StatePath_Leaf :: StatePath

--- Transition

foreign import data Transition :: Type

foreign import data Transition_ :: StatePath -> StatePath -> Transition

--- Syntax and Shorthands

infixr 6 type Transition_ as >>

type At = StatePath_Leaf

type Cases = StatePath_Cases

type Fields = StatePath_Fields

--- MatchStatePath

class
  MatchStatePath
    (path :: StatePath)
    (sta :: Type)
    (staSub1 :: Type)
    (staSub2 :: Type)
  | path sta -> staSub1 staSub2
  where
  stateGuard :: Proxy path -> sta -> Maybe staSub1
  stateExpand :: Proxy path -> staSub2 -> sta

instance
  ( FilterRow casesRL staGt staLt1
  , RowToList cases casesRL
  , V.Contractable staGt staLt1
  , Union staLt2 x staGt
  ) =>
  MatchStatePath (StatePath_Cases cases) (Variant staGt) (Variant staLt1) (Variant staLt2)
  where
  stateGuard _ = V.contract
  stateExpand _ = V.expand

--- ValidProtocol

class
  ValidProtocol (ptc :: Protocol) (msg :: Type) (sta :: Type)
  | ptc -> msg sta

instance
  ( RowToList trans transRL
  , ValidProtocolRL transRL msg sta
  ) =>
  ValidProtocol (Protocol_ trans) (Variant msg) sta

---

class
  ValidProtocolRL (ptc :: RowList Transition) (msg :: Row Type) (sta :: Type)
  | ptc -> msg sta

instance ValidProtocolRL RL.Nil () sta

instance
  ( Row.Cons s t' msg' msg
  , ValidProtocolRL rl msg' sta
  , ValidTransition t sta
  ) =>
  ValidProtocolRL (RL.Cons s t rl) msg sta

--- 

class
  ValidTransition (trans :: Transition) (sta :: Type)
  | trans -> sta

instance
  ( ValidStatePath sp1 sta
  , ValidStatePath sp2 sta
  ) =>
  ValidTransition (Transition_ sp1 sp2) sta

--

class ValidStatePath (sp :: StatePath) (sta :: Type) | sp -> sta

instance
  ( RowToList cases casesRL
  , ValidStatePathRL casesRL sta
  ) =>
  ValidStatePath (StatePath_Cases cases) (Variant sta)

instance
  ( RowToList cases casesRL
  , ValidStatePathRL casesRL sta
  ) =>
  ValidStatePath (StatePath_Fields xs) (Record sta)

instance
  ValidStatePath StatePath_Leaf sta

-- 

class ValidStatePathRL (sp :: RowList StatePath) (sta :: Row Type) | sp -> sta

instance ValidStatePathRL RL.Nil sta

instance
  ( ValidStatePathRL rl sta'
  , ValidStatePath t t'
  , Row.Cons s t' sta' sta
  ) =>
  ValidStatePathRL (RL.Cons s t rl) sta

--- FilterRow

class
  FilterRow
    (casesRL :: RowList StatePath)
    (rin :: Row Type)
    (rout :: Row Type)
  | casesRL rin -> rout

instance FilterRow RL.Nil rin ()

instance
  ( FilterRow rl rin rout'
  , Row.Cons sym a rinX rin
  , Row.Cons sym a rout' rout
  ) =>
  FilterRow (RL.Cons sym StatePath_Leaf rl) rin rout

--- SingleCaseVariant

class SingleCaseVariant (r :: Row Type) (a :: Type) | r -> a where
  unSingleCase :: Variant r -> a
  reSingleCase :: a -> Variant r

instance
  ( RowToList r (RL.Cons sym a RL.Nil)
  , Row.Cons sym a () r
  , IsSymbol sym
  ) =>
  SingleCaseVariant r a where
  unSingleCase = V.case_ # V.on (Proxy :: _ sym) identity
  reSingleCase = V.inj (Proxy :: _ sym)

--- Util

toOne :: forall r a b. SingleCaseVariant r a => (b -> a) -> (b -> Variant r)
toOne f = f >>> reSingleCase

fromOne :: forall r a b. SingleCaseVariant r a => (a -> b) -> (Variant r -> b)
fromOne f = unSingleCase >>> f

oneToOne
  :: forall ra rb a b
   . SingleCaseVariant rb b
  => SingleCaseVariant ra a
  => (a -> b)
  -> (Variant ra -> Variant rb)
oneToOne = fromOne <<< toOne