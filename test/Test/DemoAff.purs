-- | The same demo, with the operations written in `Aff`.
-- |
-- | This is what the monad parameter is for. The operations need `Aff`
-- | because real ones make requests, and writing them in `Effect` means
-- | a `launchAff_` inside each one - which puts fibers in the module
-- | that was supposed to only know about states and messages.
-- |
-- | The dispatchers record still hands `Effect` to React, because React
-- | calls it and expects something runnable. Where `Aff` becomes
-- | `Effect` is one visible line per field, at the edge, instead of
-- | being buried in every operation.
module Test.DemoAff where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Test.Demo (Msg(..), State, init, update)
import Stadium.Core as Stadium
import Stadium.React (useStateMachineSimple)

-- | What the machine may be told to do, in the monad it is written in.
type Ops =
  { countUp :: Aff Unit
  , countDown :: Aff Unit
  }

ops :: Stadium.DispatcherApi Aff Msg State Unit -> Ops
ops api =
  { countUp: api.emitMsg CountUp
  , countDown: api.emitMsg CountDown
  }

-- | What React is handed.
type Dispatchers =
  { countUp :: Effect Unit
  , countDown :: Effect Unit
  }

useDemoStateMachine :: Effect { state :: State, dispatch :: Dispatchers }
useDemoStateMachine = useStateMachineSimple
  { update
  , init
  , dispatchers: ops >>> run
  }
  where
  run :: Ops -> Dispatchers
  run o = { countUp: launchAff_ o.countUp, countDown: launchAff_ o.countDown }
