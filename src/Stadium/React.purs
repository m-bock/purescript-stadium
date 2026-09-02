module Stadium.React where

import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Stadium.Core (PursConfig, PursConfigSimple, defaultPursConfig, fromSimplePursConfig, TsApi, mkTsApi)

foreign import useStateMachineImpl
  :: forall msg pubState privState disp
   . (TsApi msg pubState privState disp)
  -> Effect { state :: pubState, dispatch :: disp }

useStateMachine
  :: forall m msg state privState err disp
   . MonadEffect m
  => (PursConfig m Unit Unit Unit Unit Unit -> PursConfig m msg state privState err disp)
  -> Effect { state :: state, dispatch :: disp }
useStateMachine mkPursCfg = do
  let tsApi = mkTsApi (mkPursCfg defaultPursConfig)
  useStateMachineImpl tsApi

useStateMachineSimple
  :: forall m msg state disp
   . MonadEffect m
  => PursConfigSimple m msg state disp
  -> Effect { state :: state, dispatch :: disp }
useStateMachineSimple cfg = do
  let tsApi = mkTsApi (fromSimplePursConfig cfg)
  useStateMachineImpl tsApi