module Stadium.DotGraph
  ( module Exp
  , toDotGraph
  ) where

import Prelude

import Data.Array as Arr
import Data.DotLang (Definition(..), Graph(..), (=*>))
import Data.DotLang as Dot
import Data.DotLang.Attr.Global as G
import Data.DotLang.Attr.Node as N
import Data.DotLang.Class (toText) as Exp
import Data.Foldable (fold)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as Str
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested ((/\))
import Stadium.Reflection as SDR

type Path = Array PathSeg

data Direction = D_Outgoing | D_Incoming
data NodeId
  = NI_MsgLabel String
  | NI_MsgPath String Direction Int Path
  | NI_StateTree Path

data ClusterId = ClusterId Path

data PathSeg
  = PS_Case String
  | PS_Field String

toDotGraph :: SDR.ValidProtocol -> Dot.Graph
toDotGraph vptc = DiGraph $ fold
  [ pure $ stateTree [] sp
  , mp # Map.toUnfoldable >>= uncurry plotMessage
  ]
  where
  SDR.Protocol_ mp = SDR.unValidProtocol vptc
  sp = SDR.getMergedStatePath vptc

getStartPath :: Path -> Maybe Path
getStartPath path =
  Arr.findLastIndex isField path
    <#> \ix -> Arr.slice 0 ix path

plotMessage :: String -> SDR.Transition -> Array Dot.Definition
plotMessage msgId (SDR.Transition_ src tgt) = fold
  [ pure $ Dot.node (printId $ NI_MsgLabel msgId)
      [ N.Shape N.Note
      , N.Label $ N.TextLabel $ msgId
      ]
  , plotMsgPaths msgId D_Incoming src
  , plotMsgPaths msgId D_Outgoing tgt
  ]

plotMsgPaths :: String -> Direction -> SDR.StatePath Void -> Array Dot.Definition
plotMsgPaths msgName dir sp = SDR.distributeCases sp
  # mapWithIndex (\ix -> plotMsgPath msgName dir ix [])
  # join

plotMsgPath :: forall leaf. String -> Direction -> Int -> Path -> SDR.OneStatePath leaf -> Array Dot.Definition
plotMsgPath msgId dir ix path = case _ of
  SDR.OneStatePath_Fields _ -> []
  SDR.OneStatePath_Case (Tuple k v) ->
    plotMsgPath msgId dir ix (path `Arr.snoc` PS_Case k) v
  SDR.OneStatePath_Leaf _ -> [ printId src =*> printId tgt $ [] ]
    where
    ida = case getStartPath path of
      Nothing -> NI_MsgLabel msgId
      Just path' -> NI_MsgPath msgId dir ix path'
    idb = NI_StateTree path

    (src /\ tgt) = case dir of
      D_Outgoing -> ida /\ idb
      D_Incoming -> idb /\ ida

isField :: PathSeg -> Boolean
isField (PS_Field _) = true
isField _ = false

printPathSeg' :: PathSeg -> String
printPathSeg' = case _ of
  PS_Case x -> x
  PS_Field x -> x

getName :: Path -> Maybe String
getName p = Arr.last p <#> printPathSeg'

stateTree :: forall leaf. Path -> SDR.StatePath leaf -> Dot.Definition
stateTree path = case _ of
  SDR.StatePath_Leaf _ -> Dot.node (printId $ NI_StateTree path)
    [ N.Shape N.Box
    , N.Style N.Rounded
    , N.Label $ N.TextLabel $ fromMaybe "" $ getName path
    ]
  SDR.StatePath_Cases xs ->
    let
      nodes = xs
        # (Map.toUnfoldable :: _ -> Array _)
        <#> (\(Tuple k v) -> stateTree (path `Arr.snoc` PS_Case k) v)

      name = fromMaybe "" $ getName path
      id = ClusterId path
    in
      Dot.Subgraph (Just $ printId id)
        ( nodes <>
            [
              -- Dot.node (printId indexId)
              --   [ N.Style N.Invis
              --   , N.Width 0
              --   ]
              Global
                [ G.Label name
                , G.Style G.Rounded
                , G.LabelJust G.L
                ]
            ]
        )
  SDR.StatePath_Fields _ -> Dot.Subgraph (Just "") []

class PrintId a where
  printId :: a -> String

instance PrintId PathSeg where
  printId = case _ of
    PS_Case x -> "case_" <> x
    PS_Field x -> "field_" <> x

instance PrintId Path where
  printId xs = Str.joinWith "__" $ printId <$> xs

instance PrintId NodeId where
  printId x = Str.joinWith "__" case x of
    NI_MsgLabel id ->
      [ "msgLabel", id ]
    NI_MsgPath id dir ix path ->
      [ "msgPath", id, printId dir, show ix, printId path ]
    NI_StateTree path ->
      [ "stateTree", printId path ]

instance PrintId Direction where
  printId = case _ of
    D_Incoming -> "incoming"
    D_Outgoing -> "outgoing"

instance PrintId ClusterId where
  printId (ClusterId path) = "cluster" <> "_" <> printId path
