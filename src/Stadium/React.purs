module Stadium.React where

import Data.Unit (Unit)
import Effect (Effect)
import Stadium.Core (PursConfig, PursConfigSimple, defaultPursConfig, fromSimplePursConfig, TsApi, mkTsApi)

foreign import useStateMachineImpl
  :: forall msg pubState privState disp
   . (TsApi msg pubState privState disp)
  -> Effect { state :: pubState, dispatch :: disp }

useStateMachine
  :: forall msg state privState err disp
   . (PursConfig Unit Unit Unit Unit Unit -> PursConfig msg state privState err disp)
  -> Effect { state :: state, dispatch :: disp }
useStateMachine mkPursCfg = do
  let tsApi = mkTsApi (mkPursCfg defaultPursConfig)
  useStateMachineImpl tsApi

useStateMachineSimple
  :: forall msg state disp
   . PursConfigSimple msg state disp
  -> Effect { state :: state, dispatch :: disp }
useStateMachineSimple cfg = do
  let tsApi = mkTsApi (fromSimplePursConfig cfg)
  useStateMachineImpl tsApi