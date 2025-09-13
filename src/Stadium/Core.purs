module Stadium.Core
  ( FullState(..)
  , mkTsApi
  , DispatcherApi
  , TsApi(..)
  , TsStateHandle(..)
  , logJson
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Codec (encode)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Lens (Lens, over, set)
import Data.Lens.Record as LensRecord
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Prim.Row as Row
import Type.Prelude (Proxy(..))

-- Public state

newtype FullState msg pubState privState = FullState
  { pubState :: pubState
  , privState :: privState
  , history :: Array { msg :: msg, pubState :: pubState }
  , historyIndex :: Number
  }

type DispatcherApi msg pubState privState =
  { emitMsg :: msg -> Effect Unit
  , emitMsgCtx :: String -> msg -> Effect Unit
  , readPubState :: Effect pubState
  , readPrivState :: Effect privState
  , updatePrivState :: (privState -> privState) -> Effect Unit
  }

---

type PursConfig msg pubState privState err disp =
  { updatePubState :: msg -> pubState -> Either String pubState
  , dispatchers :: DispatcherApi msg pubState privState -> disp
  , printError :: err -> String
  , initPubState :: pubState
  , initPrivState :: privState
  , encodeJsonPubState :: pubState -> Json
  , encodeMsg :: msg -> { tag :: String, args :: Json }
  }

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

mkTsApi :: forall msg pubState privState err disp. PursConfig msg pubState privState err disp -> TsApi msg pubState privState disp
mkTsApi { initPubState, initPrivState, dispatchers, updatePubState, encodeJsonPubState, encodeMsg } =
  TsApi
    { dispatchers: mkDispatcherApi >>> dispatchers
    , initState
    , timeTravel: mkEffectFn1 \n -> pure unit
    }
  where
  initState :: FullState msg pubState privState
  initState = FullState
    { history: []
    , historyIndex: 0.0
    , pubState: initPubState
    , privState: initPrivState
    }

  mkDispatcherApi :: TsStateHandle (FullState msg pubState privState) -> DispatcherApi msg pubState privState
  mkDispatcherApi (TsStateHandle ts) =
    { emitMsg: emitMsg Nothing
    , emitMsgCtx: \ctx -> emitMsg (Just ctx)
    , readPubState: do
        FullState st <- ts.readState
        pure st.pubState
    , readPrivState: do
        FullState st <- ts.readState
        pure st.privState
    , updatePrivState: \f ->
        ts.updateState (\(FullState state) -> pure (FullState state { privState = f state.privState }))
    }
    where
    emitMsg :: Maybe String -> msg -> Effect Unit
    emitMsg mayCtx msg = ts.updateState
      ( \(FullState state) -> case updatePubState msg state.pubState of
          Left err -> do
            log err
            pure (FullState state)
          Right newState -> do
            let { tag, args } = encodeMsg msg

            logJson
              ( [ encode CA.string ("%c" <> tag)
                , encode CA.string "color: white; background: #cc8a21; padding: 2px 4px;"
                ] <> (maybe [] (\v -> [ encode CA.string ("@" <> v) ]) mayCtx) <>
                  [ args
                  , encode CA.string "\nnewState"
                  , encodeJsonPubState newState
                  ]
              )

            pure
              ( state
                  # set (prop @"pubState") newState
                  # set (prop @"historyIndex") (state.historyIndex + 1.0)
                  # over (prop @"history") (\xs -> xs <> [ { msg, pubState: newState } ])
                  # FullState
              )
      )

---

foreign import logJson :: Array Json -> Effect Unit

prop :: forall @l r1 r2 r a b. IsSymbol l => Row.Cons l a r r1 => Row.Cons l b r r2 => Lens (Record r1) (Record r2) a b
prop = LensRecord.prop (Proxy :: _ l)