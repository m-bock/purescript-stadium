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
    # RM.writeGraphFiles "assets" "state-3"

  (Proxy3 :: _ RM.Protocol'4 RM.Msg'4 RM.State'4)
    # mkValidProtocol
    # RM.writeGraphFiles "assets" "state-4"

