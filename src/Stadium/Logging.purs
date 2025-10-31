module Stadium.Logging where

import Prelude

-- type PursDebugConfig msg pubState err =
--   { encodeJsonPubState :: Maybe (pubState -> Json)
--   , encodeMsg :: Maybe (msg -> Json)
--   , debugMsg :: Maybe (Json -> Either err { tag :: String, values :: Array Json })
--   , printError :: Maybe (err -> String)
--   }

-- defaultPursDebugConfig :: PursDebugConfig Unit Unit Unit
-- defaultPursDebugConfig =
--   { encodeJsonPubState: Nothing
--   , encodeMsg: Nothing
--   , debugMsg: Nothing
--   , printError: Nothing
--   }

-- case cfg.encodeJsonPubState, cfg.encodeMsg of
--   (Just encodeJsonPubState, Just encodeMsg) -> do

--     let
--       json = encodeMsg msg
--       { tag, values } = fromRight { tag: "Unknown", values: [ json ] } (debugMsg json)

--       valOrVals = case values of
--         [ val ] -> encode CA.json val
--         vals -> encode (CA.array CA.json) vals

--     logJson
--       ( [ encode CA.string ("%c" <> tag <> maybe "" (\v -> "%c@" <> v) mayCtx)
--         , encode CA.string "color: white; background: #cc8a21; padding: 2px 4px;"
--         ] <> maybe [] (\v -> [ encode CA.string "color: white; background:rgb(248, 98, 38); padding: 2px 4px;" ]) mayCtx <>
--           [ valOrVals
--           , encode CA.string "\nnewState"
--           , encodeJsonPubState newState
--           ]
--       )
--   (_, _) -> pure unit
