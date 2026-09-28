-- | What `applyMsg` is for.
-- |
-- | A dispatcher that has to know where the machine landed used to
-- | emit and then read, which is two steps with a gap between them -
-- | and in async code something else can land in that gap. `applyMsg`
-- | is the same emit with the answer kept: what comes back is the
-- | state this message produced.
module Test.DemoApply where

import Prelude

import Effect.Aff (Aff)
import Stadium.Core as Stadium
import Test.Demo (Msg(..), State)

type Ops =
  { countUp :: Aff Unit
  , countUpAndSay :: Aff String
  }

ops :: Stadium.DispatcherApi Aff Msg State Unit -> Ops
ops api =
  { countUp:
      api.emitMsg CountUp

  , countUpAndSay: do
      now <- api.applyMsg CountUp

      pure ("counted up to " <> show now.count)
  }
