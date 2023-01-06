module Stadium.Reflection
  ( FnReflectRow(..)
  , FnReflectStatePath(..)
  , FnReflectTransition(..)
  , Protocol(..)
  , StatePath(..)
  , StrMap
  , Transition(..)
  , class ReflectProtocol
  , class ReflectTransition
  , class ReflectRow
  , class ReflectStatePath
  , class MkValidProtocol
  , mergeStatePath
  , reflectProtocol
  , reflectTransition
  , reflectRow
  , reflectStatePath
  , getMergedStatePath
  , ValidProtocol
  , mkValidProtocol
  , unValidProtocol
  , distributeCases
  , OneStatePath(..)
  , Proxy3(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (foldM)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Heterogeneous.Mapping (class Mapping, mapping)
import Partial.Unsafe (unsafeCrashWith)
import Prim.RowList (class RowToList)
import Stadium as SD
import Type.Proxy (Proxy(..))

type StrMap = Map String

newtype Protocol = Protocol_ (StrMap Transition)

data Transition = Transition_ (StatePath Void) (StatePath Void)

data StatePath leaf
  = StatePath_Fields (StrMap (StatePath Void))
  | StatePath_Cases (StrMap (StatePath Unit))
  | StatePath_Leaf leaf

data OneStatePath leaf
  = OneStatePath_Fields (StrMap (OneStatePath Void))
  | OneStatePath_Case (Tuple String (OneStatePath Unit))
  | OneStatePath_Leaf leaf

---

data Proxy3 :: forall k1 k2 k3. k1 -> k2 -> k3 -> Type
data Proxy3 a b c = Proxy3

--- ValidProtocol

newtype ValidProtocol = ValidProtocol Protocol

class MkValidProtocol (ptc :: SD.Protocol) (msg :: Type) (sta :: Type) where
  mkValidProtocol :: Proxy3 ptc msg sta -> ValidProtocol

instance
  ( SD.ValidProtocol ptc msg sta
  , ReflectProtocol ptc
  ) =>
  MkValidProtocol ptc msg sta
  where
  mkValidProtocol _ = ValidProtocol $ reflectProtocol (Proxy :: _ ptc)

unValidProtocol :: ValidProtocol -> Protocol
unValidProtocol (ValidProtocol x) = x

---

getMergedStatePath :: ValidProtocol -> StatePath Void
getMergedStatePath (ValidProtocol ptc) =
  getMergedStatePath' ptc
    # fromMaybe' (\_ -> unsafeCrashWith "invalid protocol")

getMergedStatePath' :: Protocol -> Maybe (StatePath Void)
getMergedStatePath' (Protocol_ mp) = mp
  # Map.values
  >>= (\(Transition_ x y) -> List.fromFoldable [ x, y ])
  # foldM mergeStatePath (StatePath_Cases Map.empty)

mergeStatePath :: forall leaf. StatePath leaf -> StatePath leaf -> Maybe (StatePath leaf)
mergeStatePath x y =
  case x, y of
    StatePath_Cases xs, StatePath_Cases ys -> StatePath_Cases <$> mergeStatePaths xs ys
    StatePath_Fields xs, StatePath_Fields ys -> StatePath_Fields <$> mergeStatePaths xs ys
    StatePath_Leaf _, o -> Just o
    o, StatePath_Leaf _ -> Just o
    _, _ -> Nothing

mergeStatePaths :: forall leaf. StrMap (StatePath leaf) -> StrMap (StatePath leaf) -> Maybe (StrMap (StatePath leaf))
mergeStatePaths xs ys = ado
  both <- Map.intersectionWith mergeStatePath xs ys # sequence
  let onlyFst = Map.difference xs ys
  let onlySnd = Map.difference ys xs
  in Map.unions [ both, onlyFst, onlySnd ]

---

distributeCases :: forall leaf. StatePath leaf -> Array (OneStatePath leaf)
distributeCases = case _ of
  StatePath_Fields mp -> mp
    <#> distributeCases
    # sequence
    <#> OneStatePath_Fields
  StatePath_Cases mp -> mp # Map.toUnfoldable >>=
    \(Tuple k v) -> (OneStatePath_Case <<< Tuple k) <$> distributeCases v
  StatePath_Leaf x -> [ OneStatePath_Leaf x ]

--- Instances

derive instance Generic (StatePath leaf) _
derive instance Generic Protocol _
derive instance Generic Transition _

derive instance Eq leaf => Eq (StatePath leaf)
derive instance Eq Transition
derive instance Eq Protocol

instance Show Transition where
  show = genericShow

instance Show Protocol where
  show = genericShow

instance Show leaf => Show (StatePath leaf) where
  show x = genericShow x

---

data FnReflectTransition = FnReflectTransition

data FnReflectStatePath :: forall k. k -> Type
data FnReflectStatePath leaf = FnReflectStatePath (Proxy leaf)

data FnReflectRow v = FnReflectRow v

---

instance (ReflectTransition a) => Mapping FnReflectTransition (Proxy a) Transition where
  mapping _ = reflectTransition

instance (ReflectStatePath a leaf) => Mapping (FnReflectStatePath leaf) (Proxy a) (StatePath leaf) where
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
  ( ReflectStatePath src Void
  , ReflectStatePath tgt Void
  ) =>
  ReflectTransition (SD.Transition_ src tgt)
  where
  reflectTransition _ = Transition_
    (reflectStatePath (Proxy :: _ src))
    (reflectStatePath (Proxy :: _ tgt))

--- ReflectStatePath
class ReflectStatePath (a :: SD.StatePath) (leaf :: Type) where
  reflectStatePath :: Proxy a -> StatePath leaf

instance ReflectStatePath SD.StatePath_Leaf Unit where
  reflectStatePath _ = StatePath_Leaf unit

instance
  ( ReflectRow (FnReflectStatePath Void) r (StatePath Void)
  ) =>
  ReflectStatePath (SD.StatePath_Fields r) leaf
  where
  reflectStatePath _ = StatePath_Fields $
    reflectRow (FnReflectStatePath (Proxy :: _ Void)) (Proxy :: _ r)

instance
  ( ReflectRow (FnReflectStatePath Unit) r (StatePath Unit)
  ) =>
  ReflectStatePath (SD.StatePath_Cases r) leaf
  where
  reflectStatePath _ = StatePath_Cases $
    reflectRow (FnReflectStatePath (Proxy :: _ Unit)) (Proxy :: _ r)

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
