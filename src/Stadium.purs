module Stadium
  ( Case
  , Case_Leaf
  , Case_StatePath
  , Field
  , Field_Cases
  , Field_Fields
  , Many
  , Or
  , Protocol
  , Protocol_
  , SimpleTransition'ManyToMany
  , SimpleTransition'ManyToOne
  , SimpleTransition'OneToMany
  , SimpleTransition'OneToOne
  , StatePath
  , StatePath_Cases
  , StatePath_Fields
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
  , oneToOne
  , reSingleCase
  , stateExpand
  , stateGuard
  , toOne
  , type (>>)
  , type (>>..)
  , type (||)
  , unSingleCase
  , module Exp
  )
  where

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
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Stadium.IsoVariant as Exp


--- Protocol

foreign import data Protocol :: Type

foreign import data Protocol_ :: Row Transition -> Protocol

--- StatePath

foreign import data StatePath :: Type

foreign import data StatePath_Fields :: RowList Field -> StatePath

foreign import data StatePath_Cases :: RowList Case -> StatePath

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

--- Syntax

type Many (s :: Symbol) = RL.Cons s Case_Leaf RL.Nil

type Or (rl :: RowList Case) (s :: Symbol) = RL.Cons s Case_Leaf rl

infixl 7 type Or as ||

infixr 6 type SimpleTransition'OneToOne as >>

infixr 6 type SimpleTransition'OneToMany as >>..

type SimpleTransition'ManyToMany (src :: RowList Case) (tgt :: RowList Case) =
  Transition_ (StatePath_Cases src) (StatePath_Cases tgt)

type SimpleTransition'OneToOne (src :: Symbol) (tgt :: Symbol) =
  SimpleTransition'ManyToMany
    (RL.Cons src Case_Leaf RL.Nil)
    (RL.Cons tgt Case_Leaf RL.Nil)

type SimpleTransition'OneToMany (src :: Symbol) (tgt :: RowList Case) =
  SimpleTransition'ManyToMany
    (RL.Cons src Case_Leaf RL.Nil)
    tgt

type SimpleTransition'ManyToOne (src :: RowList Case) (tgt :: Symbol) =
  SimpleTransition'ManyToMany
    src
    (RL.Cons tgt Case_Leaf RL.Nil)

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
  , V.Contractable staGt staLt1
  , Union staLt2 x staGt
  ) =>
  MatchStatePath (StatePath_Cases casesRL) (Variant staGt) (Variant staLt1) (Variant staLt2)
  where
  stateGuard _ = V.contract
  stateExpand _ = V.expand

--- FilterRow

class
  FilterRow
    (casesRL :: RowList Case)
    (rin :: Row Type)
    (rout :: Row Type)
  | casesRL rin -> rout

instance FilterRow RL.Nil rin ()

instance
  ( FilterRow rl rin rout'
  , Row.Cons sym a rinX rin
  , Row.Cons sym a rout' rout
  ) =>
  FilterRow (RL.Cons sym Case_Leaf rl) rin rout

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