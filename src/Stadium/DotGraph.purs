module Stadium.DotGraph where

import Prelude

import Data.DotLang (Graph(..))
import Data.DotLang as Dot
import Stadium.Reflection as SDR
import Unsafe.Coerce (unsafeCoerce)


toDotGraph :: SDR.Protocol -> Dot.Graph
toDotGraph _ = DiGraph []