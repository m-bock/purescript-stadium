module Stadium.Protocol.Spec where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, default, inj, on, prj)
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

--- StateMachine

foreign import data StateMachine :: Type

foreign import data StateMachine_ :: Type -> Protocol -> StateMachine

--- Protocol

foreign import data Protocol :: Type

foreign import data Protocol_ :: Row Action -> Protocol

--- Action

foreign import data Action :: Type

foreign import data Action_ :: Type -> Transition -> Action

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

--foreign import data Case_Cases :: Row Case -> Case

foreign import data Case_Leaf :: Case

foreign import data Case_StatePath :: StatePath -> Case

--- Transition

foreign import data Transition :: Type

foreign import data Transition_ :: StatePath -> StatePath -> Transition

---

class GetState (stm :: StateMachine) sta | stm -> sta

instance GetState (StateMachine_ state x) state

-- ---

class GetMsg (stm :: StateMachine) (msg :: Row Type) | stm -> msg

instance
  ( RowToList msgs msgsRL
  , GetMsgRL msgsRL r
  ) =>
  GetMsg (StateMachine_ x (Protocol_ msgs)) r



class GetMsgRL (msgsRL :: RowList Action) (r :: Row Type) | msgsRL -> r

instance GetMsgRL Nil ()

instance (GetMsgRL rl r', Cons sym a r' r) => GetMsgRL (Cons sym (Action_ a x) rl) r

-- --

class GetCases (stm :: StateMachine) (sta :: Type) (cases :: Row Type) (msg :: Row Type) | stm sta -> cases msg where
  getCases :: Proxy stm -> sta -> Proxy msg -> Record cases -> Variant msg -> Maybe sta

instance
  ( RowToList msgs msgsRL
  , GetCasesRL msgsRL sta cases msg
  ) =>
  GetCases (StateMachine_ sta (Protocol_ msgs)) sta cases msg where
  getCases stm sta _ cases msg = getCasesRL (Proxy :: _ msgsRL) sta cases msg

class
  GetCasesRL (msgsRL :: RowList Action) (sta :: Type) (cases :: Row Type) (msg :: Row Type)
  | msgsRL sta -> cases msg
  where
  getCasesRL :: Proxy msgsRL -> sta -> Record cases -> Variant msg -> Maybe sta

instance GetCasesRL Nil sta () () where
  getCasesRL _ _ _ _ = Nothing

instance
  ( GetCasesRL rl sta cases' msg'
  , Cons sym (a -> sta -> sta) cases' cases
  , Cons sym a msg' msg
  , IsSymbol sym
  ) =>
  GetCasesRL (Cons sym (Action_ a x) rl) sta cases msg where
  getCasesRL _ sta cases msg =
    ( default Nothing #
        on (Proxy :: _ sym) (\data_ -> Just $ case' data_ sta)
    ) msg
    where
    case' = Record.get (Proxy :: _ sym) cases

---

class
  MkReducer (stm :: StateMachine) msg sta cases
  | stm -> msg sta cases
  where
  mkReducer :: Proxy stm -> cases -> msg -> sta -> Maybe sta

instance
  ( GetState stm sta
  , GetCases stm sta cases msg
  ) =>
  MkReducer stm (Variant msg) sta (Record cases) where
  mkReducer _ cases msg sta = getCases (Proxy :: _ stm) sta (Proxy :: _ msg) cases msg

-- where
-- x :: Unit
-- x = Record.get (Proxy :: _ sym) cases

---
-- forall msg. GetMsg MyStateMachine msg => Variant msg

-- type T cases msg = GetCases MyStateMachine MyState cases msg => Variant msg

myReducer :: _ -> MyState -> Maybe MyState
myReducer = mkReducer (Proxy :: _ MyStateMachine)
  { switchOn: \x st -> st
  , switchOff: \x st -> st
  }

x :: Maybe MyState
x =
  (inj (Proxy :: _ "off") unit)
    # myReducer (inj (Proxy :: _ "switchOn") unit)
    >>= myReducer (inj (Proxy :: _ "switchOff") unit)

--- Test

type MyStateMachine = StateMachine_ MyState MyProtocol

type MyState = Variant (on :: Unit, off :: Unit)

type MyProtocol = Protocol_
  ( switchOn ::
      Action_ Unit
        ( Transition_
            (StatePath_Cases (off :: Case_Leaf))
            (StatePath_Cases (on :: Case_Leaf))
        )
  , switchOff ::
      Action_ Unit
        ( Transition_
            (StatePath_Cases (on :: Case_Leaf))
            (StatePath_Cases (off :: Case_Leaf))
        )
  )

-- type T = Protocol_
--   ( actionA ::
--       Action_ Int
--         ( Transition_
--             ( StatePath_Fields
--                 ( a ::
--                     Field_Cases
--                       ( foo :: Case_Leaf
--                       , bar :: Case_StatePath (StatePath_Cases (a :: Case_Leaf))
--                       )
--                 , b :: Field_Fields (c :: Field_Cases (foo :: Case_Leaf))
--                 )
--             )
--             ( StatePath_Fields
--                 ( a :: Field_Cases (foo :: Case_Leaf)

--                 )
--             )
--         )
--   )

