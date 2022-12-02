module Stadium.Reflection
  ( FnReflectRow(..)
  , FnReflectStatePath(..)
  , Protocol(..)
  , StatePath(..)
  , StrMap
  , Transition(..)
  , class ReflectProtocol
  , class ReflectRow
  , class ReflectStatePath
  , mergeStatePath
  , reflectProtocol
  , reflectRow
  , reflectStatePath
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class Mapping, mapping)
import Prim.RowList (class RowToList)
import Stadium as SD
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type StrMap = Map String

newtype Protocol = Protocol_ (StrMap Transition)

data Transition = Transition_ StatePath StatePath

data StatePath
  = StatePath_Fields (StrMap StatePath)
  | StatePath_Cases (StrMap StatePath)
  | StatePath_Leaf

---

mergeStatePath :: StatePath -> StatePath -> Maybe StatePath
mergeStatePath x y =
  case x, y of
    StatePath_Cases xs, StatePath_Cases ys -> StatePath_Cases <$> mergeStatePaths xs ys
    StatePath_Fields xs, StatePath_Fields ys -> StatePath_Fields <$> mergeStatePaths xs ys
    StatePath_Leaf, o -> Just o
    o, StatePath_Leaf -> Just o
    _, _ -> Nothing

mergeStatePaths :: StrMap StatePath -> StrMap StatePath -> Maybe (StrMap StatePath)
mergeStatePaths xs ys = ado
  both <- Map.intersectionWith mergeStatePath xs ys # sequence
  let onlyFst = Map.difference xs ys
  let onlySnd = Map.difference ys xs
  in Map.unions [ both, onlyFst, onlySnd ]

--- Instances

derive instance Generic StatePath _
derive instance Generic Protocol _
derive instance Generic Transition _

derive instance Eq StatePath
derive instance Eq Transition
derive instance Eq Protocol

instance Show Transition where
  show = genericShow

instance Show Protocol where
  show = genericShow

instance Show StatePath where
  show x = genericShow x

---

data FnReflectTransition = FnReflectTransition
data FnReflectStatePath = FnReflectStatePath

data FnReflectRow v = FnReflectRow v

---

instance (ReflectTransition a) => Mapping FnReflectTransition (Proxy a) Transition where
  mapping _ = reflectTransition

instance (ReflectStatePath a) => Mapping FnReflectStatePath (Proxy a) StatePath where
  mapping _ = reflectStatePath

--- ReflectProtocol

class ReflectProtocol (a :: SD.Protocol) where
  reflectProtocol :: Proxy a -> Protocol

instance
  ( ReflectRow FnReflectTransition r Transition
  ) =>
  ReflectProtocol (SD.Protocol_ r)
  where
  reflectProtocol _ = Protocol_ $
    reflectRow FnReflectTransition (Proxy :: _ r)

--- ReflectTransition
class ReflectTransition (a :: SD.Transition) where
  reflectTransition :: Proxy a -> Transition

instance
  ( ReflectStatePath src
  , ReflectStatePath tgt
  ) =>
  ReflectTransition (SD.Transition_ src tgt)
  where
  reflectTransition _ = Transition_
    (reflectStatePath (Proxy :: _ src))
    (reflectStatePath (Proxy :: _ tgt))

--- ReflectStatePath
class ReflectStatePath (a :: SD.StatePath) where
  reflectStatePath :: Proxy a -> StatePath

instance ReflectStatePath SD.StatePath_Leaf where
  reflectStatePath _ = StatePath_Leaf

instance
  ( ReflectRow FnReflectStatePath r StatePath
  ) =>
  ReflectStatePath (SD.StatePath_Fields r)
  where
  reflectStatePath _ = StatePath_Fields $
    reflectRow FnReflectStatePath (Proxy :: _ r)

instance
  ( ReflectRow FnReflectStatePath r StatePath
  ) =>
  ReflectStatePath (SD.StatePath_Cases r)
  where
  reflectStatePath _ = StatePath_Cases $
    reflectRow FnReflectStatePath (Proxy :: _ r)

--- ReflectRow

class ReflectRow :: forall k. Type -> Row k -> Type -> Constraint
class ReflectRow fv r v where
  reflectRow :: fv -> Proxy r -> StrMap v

instance
  ( RowToList r rl
  , HFoldlWithIndex (FnReflectRow fv) (StrMap v) (Proxy rl) (StrMap v)
  ) =>
  ReflectRow fv r v
  where
  reflectRow fv _ =
    hfoldlWithIndex (FnReflectRow fv) (Map.empty :: StrMap v) (Proxy :: _ rl)

instance
  ( IsSymbol sym
  , Mapping vv (Proxy a) v
  ) =>
  FoldingWithIndex (FnReflectRow vv) (Proxy sym) (StrMap v) (Proxy a) (StrMap v)
  where
  foldingWithIndex (FnReflectRow vv) s xs x =
    Map.insert (reflectSymbol s) (mapping vv x) xs
