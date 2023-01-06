module Stadium.Controllers.Pure where

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
import Stadium (class MatchStatePath, Protocol, Protocol_, Transition, Transition_, stateExpand, stateGuard)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

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

    caseHandler data'
      | Just staSrc <- stateGuard prxPathSrc sta =
          Just $ stateExpand prxPathSrc $ chosenCase data' staSrc
    caseHandler _ = Nothing

    chosenCase = Record.get prop cases
    prop = Proxy :: _ sym
    prxPathSrc = Proxy :: _ pathSrc

--- MkReducer

class
  MkReducer (ptc :: Protocol) (msg :: Type) (sta :: Type) (cases :: Row Type)
  | ptc msg sta -> cases
  where
  mkReducer :: Proxy ptc -> Record cases -> msg -> sta -> sta

instance
  ( GetCases transRL msg sta cases
  , RowToList trans transRL
  ) =>
  MkReducer (Protocol_ trans) (Variant msg) sta cases
  where
  mkReducer _ cases msg sta =
    getCases (Proxy :: _ transRL) cases msg sta
      # fromMaybe sta

--- Internal Util

pick :: forall r2 rx r1. Union r2 rx r1 => { | r1 } -> { | r2 }
pick = unsafeCoerce
