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
  , class GetCases
  , class MatchStatePath
  , class MkReducer
  , class SingleCaseVariant
  , fromOne
  , getCases
  , mkReducer
  , module Exp
  , oneToOne
  , reSingleCase
  , stateExpand
  , stateGuard
  , toOne
  , type (>>)
  , unSingleCase
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row (class Union)
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Stadium.IsoVariant as Exp
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

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

--- GetCases

class
  GetCases
    (trans :: RowList Transition)
    (msg :: Row Type)
    (sta :: Type)
    (cases :: Row Type)
  | trans sta msg -> cases
  where
  getCases :: Proxy trans -> Record cases -> Variant msg -> sta -> Maybe sta

instance GetCases RL.Nil msg sta () where
  getCases _ _ _ _ = Nothing

instance
  ( GetCases rl msg sta cases'
  , Row.Cons sym (a -> staSrc -> staTgt) cases' cases
  , Row.Cons sym a msg' msg
  , IsSymbol sym
  , MatchStatePath pathSrc sta staSrc staTgt
  , MatchStatePath pathTgt sta staTgt staSrc
  , Union cases' cases'x cases
  ) =>
  GetCases (RL.Cons sym (Transition_ pathSrc pathTgt) rl) msg sta cases
  where
  getCases _ cases msg sta =
    tail <|> head

    where
    tail = getCases (Proxy :: _ rl) (pick cases :: { | cases' }) msg sta

    head = msg #
      (V.default Nothing # V.on prop caseHandler)

    caseHandler data' | Just staSrc <- stateGuard prxPathSrc sta =
      Just $ stateExpand prxPathSrc $ chosenCase data' staSrc
    caseHandler _ = Nothing

    chosenCase = Record.get prop cases
    prop = Proxy :: _ sym
    prxPathSrc = Proxy :: _ pathSrc

--- MkReducer

class
  MkReducer (ptc :: Protocol) (msg :: Row Type) (sta :: Type) (cases :: Row Type)
  | ptc msg sta -> cases
  where
  mkReducer :: Proxy ptc -> Record cases -> Variant msg -> sta -> sta

instance
  ( GetCases transRL msg sta cases
  , RowToList trans transRL
  ) =>
  MkReducer (Protocol_ trans) msg sta cases
  where
  mkReducer _ cases msg sta =
    getCases (Proxy :: _ transRL) cases msg sta
      # fromMaybe sta

--- Internal Util

pick :: forall r2 rx r1. Union r2 rx r1 => { | r1 } -> { | r2 }
pick = unsafeCoerce

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