module Test.RunReadme where

import Prelude

import Effect (Effect)
import Node.ChildProcess (defaultSpawnOptions, spawn)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import Node.Path (FilePath)
import Stadium.DotGraph (toDotGraph, toText)
import Stadium.Reflection (Proxy3(..), ValidProtocol, mkValidProtocol)
import Test.Readme as RM

main :: Effect Unit
main = do
  (Proxy3 :: _ RM.Protocol'3 RM.Msg'3 RM.State'3)
    # mkValidProtocol
    # writeGraphFiles "assets" "state-3"

  (Proxy3 :: _ RM.Protocol'4 RM.Msg'4 RM.State'4)
    # mkValidProtocol
    # writeGraphFiles "assets" "state-4"

writeGraphFiles :: FilePath -> String -> ValidProtocol -> Effect Unit
writeGraphFiles dir name ptc = do
  let
    pathDot = dir <> "/" <> name <> "." <> "dot"
    pathSvg = dir <> "/" <> name <> "." <> "svg"

  toDotGraph ptc
    # toText
    # writeTextFile UTF8 pathDot

  void $ spawn "dot" [ "-Tsvg", pathDot, "-o", pathSvg ]
    defaultSpawnOptions
