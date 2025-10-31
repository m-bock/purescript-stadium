module Test.Demo where

import Prelude

import Effect (Effect)
import Stadium.Core as Stadium
import Stadium.React (useStateMachineSimple)

type State =
  { count :: Int
  }

init :: State
init = { count: 0 }

data Msg = CountUp | CountDown

update :: Msg -> State -> State
update msg state = case msg of
  CountUp -> state { count = state.count + 1 }
  CountDown -> state { count = state.count - 1 }

type Dispatchers =
  { countUp :: Effect Unit
  , countDown :: Effect Unit
  }

dispatchers :: Stadium.DispatcherApi Msg State Unit -> Dispatchers
dispatchers api =
  { countUp: api.emitMsg CountUp
  , countDown: api.emitMsg CountDown
  }

---

useDemoStateMachine :: Effect { state :: State, dispatch :: Dispatchers }
useDemoStateMachine = useStateMachineSimple
  { update
  , init
  , dispatchers
  }