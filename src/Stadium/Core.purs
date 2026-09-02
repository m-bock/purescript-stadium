module Stadium.Core
  ( DispatcherApi
  , FullState(..)
  , PursConfig
  , PursConfigSimple
  , TsApi(..)
  , TsStateHandle(..)
  , defaultDebugMsg
  , defaultPursConfig
  , fromSimplePursConfig
  , logJson
  , mkTsApi
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Lens (Lens, over, set)
import Data.Lens.Record as LensRecord
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Prim.Row as Row
import Type.Prelude (Proxy(..))

newtype FullState msg pubState privState = FullState
  { pubState :: pubState
  , privState :: privState
  , history :: Array { msg :: msg, pubState :: pubState }
  , historyIndex :: Int
  }

type DispatcherApi m msg pubState privState =
  { emitMsg :: msg -> m Unit
  , emitMsgCtx :: String -> msg -> m Unit
  , readPubState :: m pubState
  , readPrivState :: m privState
  , updatePrivState :: (privState -> privState) -> m Unit
  }

type PursConfig m msg state privState err disp =
  { update :: msg -> state -> Either err state
  , init :: state
  , dispatchers :: DispatcherApi m msg state privState -> disp
  , initPrivState :: privState
  -- Stays in `Effect`, and has to: this one is *called* by the library,
  -- from inside React's state updater. Running an arbitrary `m` back to
  -- `Effect` would need a runner, and there is none to be had here.
  , onUpdate :: Maybe String -> Either err state -> Effect Unit
  }

type PursConfigSimple m msg state disp =
  { update :: msg -> state -> state
  , init :: state
  , dispatchers :: DispatcherApi m msg state Unit -> disp
  }

defaultPursConfig :: forall m. PursConfig m Unit Unit Unit Unit Unit
defaultPursConfig =
  { update: \_ state -> Right state
  , init: unit
  , dispatchers: \_ -> unit
  , initPrivState: unit
  , onUpdate: \_ _ -> pure unit
  }

fromSimplePursConfig :: forall m msg state disp. PursConfigSimple m msg state disp -> PursConfig m msg state Unit Unit disp
fromSimplePursConfig cfg =
  { update: \msg state -> Right (cfg.update msg state)
  , init: cfg.init
  , dispatchers: cfg.dispatchers
  , initPrivState: unit
  , onUpdate: \_ _ -> pure unit
  }

defaultDebugMsg :: Json -> Either String { tag :: String, values :: Array Json }
defaultDebugMsg json = lmap CA.printJsonDecodeError do
  CA.decode (CAR.object "" { tag: CA.string, values: CA.array CA.json }) json

newtype TsStateHandle state = TsStateHandle
  { updateState :: (state -> Effect state) -> Effect Unit
  , readState :: Effect state
  }

derive instance Newtype (TsStateHandle state) _

newtype TsApi msg pubState privState disp = TsApi
  { dispatchers :: TsStateHandle (FullState msg pubState privState) -> disp
  , initState :: FullState msg pubState privState
  , timeTravel :: EffectFn1 Number Unit
  }

derive instance Newtype (TsApi msg pubState state disp) _

derive instance Newtype (FullState msg pubState privState) _

mkTsApi :: forall m msg pubState privState err disp. MonadEffect m => PursConfig m msg pubState privState err disp -> TsApi msg pubState privState disp
mkTsApi cfg =
  TsApi
    { dispatchers: mkDispatcherApi >>> cfg.dispatchers
    , initState
    , timeTravel: mkEffectFn1 \n -> pure unit
    }
  where
  initState :: FullState msg pubState privState
  initState = FullState
    { history: []
    , historyIndex: 0
    , pubState: cfg.init
    , privState: cfg.initPrivState
    }

  -- Every field is the `Effect` one lifted. That is the whole trick,
  -- and the reason this direction is free: the library only ever *hands
  -- these out*, so it needs to go outward from `Effect` and never back.
  mkDispatcherApi :: TsStateHandle (FullState msg pubState privState) -> DispatcherApi m msg pubState privState
  mkDispatcherApi (TsStateHandle ts) =
    { emitMsg: \msg -> liftEffect (emitMsg Nothing msg)
    , emitMsgCtx: \ctx msg -> liftEffect (emitMsg (Just ctx) msg)
    , readPubState: liftEffect do
        FullState st <- ts.readState
        pure st.pubState
    , readPrivState: liftEffect do
        FullState st <- ts.readState
        pure st.privState
    , updatePrivState: \f -> liftEffect
        (ts.updateState (\(FullState state) -> pure (FullState state { privState = f state.privState })))
    }
    where
    emitMsg :: Maybe String -> msg -> Effect Unit
    emitMsg mayCtx msg = ts.updateState
      ( \(FullState state) -> do
          let
            result = cfg.update msg state.pubState

          cfg.onUpdate mayCtx result

          case result of
            Left _ -> do
              pure (FullState state)
            Right newState -> do

              pure
                ( state
                    # set (prop @"pubState") newState
                    # set (prop @"historyIndex") (state.historyIndex + 1)
                    # over (prop @"history") (\xs -> xs <> [ { msg, pubState: newState } ])
                    # FullState
                )
      )

---

foreign import logJson :: Array Json -> Effect Unit

prop :: forall @l r1 r2 r a b. IsSymbol l => Row.Cons l a r r1 => Row.Cons l b r r2 => Lens (Record r1) (Record r2) a b
prop = LensRecord.prop (Proxy :: _ l)
