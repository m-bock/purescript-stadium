module Stadium.React where

import Effect (Effect)
import Stadium.Core (TsApi)

foreign import useStateMachine
  :: forall msg pubState privState disp
   . (TsApi msg pubState privState disp)
  -> Effect { state :: pubState, dispatch :: disp }
