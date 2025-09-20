module Stadium.TL where

import Prelude

import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Sum(..), from, to)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Heterogeneous.Mapping (class HMap, class Mapping, hmap)
import Prim.Row (class Union)
import Prim.Row as Row
import Record as Record
import Type.Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

data FnMapToMsgEmitter msg = FnMapToMsgEmitter { emitMsg :: msg -> Effect Unit }

instance Mapping (FnMapToMsgEmitter msg) (arg -> msg) (EffectFn1 arg Unit) where
  mapping (FnMapToMsgEmitter { emitMsg }) mkMsg = mkEffectFn1 \arg -> emitMsg (mkMsg arg)

mkCtorEmitter
  :: forall msg rin rout
   . HMap (FnMapToMsgEmitter msg) (Record rin) (Record rout)
  => { emitMsg :: msg -> Effect Unit }
  -> Record rin
  -> Record rout
mkCtorEmitter { emitMsg } = hmap (FnMapToMsgEmitter { emitMsg })

--------------------------------------------------------------------------------

class MkConstructors :: Type -> Row Type -> Constraint
class MkConstructors a r | a -> r where
  mkConstructors :: Record r

instance
  ( Generic a rep
  , MkConstructorsRep rep r'
  , HMap (FnMapFromRep a) (Record r') (Record r)
  ) =>
  MkConstructors a r where
  mkConstructors = ret
    where
    ret :: Record r
    ret = hmap (FnMapFromRep @a) r'

    r' :: Record r'
    r' = mkConstructorsRep @rep

---

class MkConstructors1 :: (Type -> Type) -> Type -> Row Type -> Constraint
class MkConstructors1 fa a r | fa a -> r where
  mkConstructors1 :: Record r

instance
  ( Generic (f a) rep
  , MkConstructorsRep rep r'
  , HMap (FnMapFromRep (f a)) (Record r') (Record r)
  ) =>
  MkConstructors1 f a r where
  mkConstructors1 = ret
    where
    ret :: Record r
    ret = hmap (FnMapFromRep @(f a)) r'

    r' :: Record r'
    r' = mkConstructorsRep @rep

---

class MkConstructorsRep :: forall k. k -> Row Type -> Constraint
class MkConstructorsRep rep r | rep -> r where
  mkConstructorsRep :: Record r

instance
  ( MkConstructorsRep lhs lhsr'
  , MkConstructorsRep rhs rhsr'
  , HMap FnMapLeft (Record lhsr') (Record lhsr)
  , HMap FnMapRight (Record rhsr') (Record rhsr)
  , Row.Union lhsr rhsr r
  ) =>
  MkConstructorsRep (Sum lhs rhs) r where
  mkConstructorsRep = ret
    where
    lhsr' :: Record lhsr'
    lhsr' = mkConstructorsRep @lhs

    lhsr :: Record lhsr
    lhsr = hmap FnMapLeft lhsr'

    rhsr' :: Record rhsr'
    rhsr' = mkConstructorsRep @rhs

    rhsr :: Record rhsr
    rhsr = hmap FnMapRight rhsr'

    ret :: Record r
    ret = Record.union lhsr rhsr

instance
  ( Row.Cons sym (Unit -> Constructor sym NoArguments) () r
  , IsSymbol sym
  ) =>
  MkConstructorsRep (Constructor sym NoArguments) r where
  mkConstructorsRep = ret
    where
    fn :: Unit -> Constructor sym NoArguments
    fn _ = Constructor NoArguments

    ret :: Record r
    ret = Record.insert (Proxy :: _ sym) fn {}

else instance
  ( Row.Cons sym (args -> Constructor sym (Argument args)) () r
  , IsSymbol sym
  ) =>
  MkConstructorsRep (Constructor sym (Argument args)) r where
  mkConstructorsRep = ret
    where
    fn :: args -> Constructor sym (Argument args)
    fn = Constructor <<< Argument

    ret :: Record r
    ret = Record.insert (Proxy :: _ sym) fn {}

data FnMapLeft = FnMapLeft

instance
  Mapping FnMapLeft (args -> a) (args -> Sum a b) where
  mapping _ x = Inl <$> x

else instance
  Mapping FnMapLeft a (Sum a b) where
  mapping _ x = Inl x

data FnMapRight = FnMapRight

instance
  Mapping FnMapRight (args -> b) (args -> Sum a b) where
  mapping _ x = Inr <$> x

else instance
  Mapping FnMapRight b (Sum a b) where
  mapping _ x = Inr x

data FnMapFromRep :: forall k. k -> Type
data FnMapFromRep a = FnMapFromRep

instance
  ( Generic a rep
  ) =>
  Mapping (FnMapFromRep a) (args -> rep) (args -> a) where
  mapping _ x = to <<< x

else instance
  ( Generic a rep
  ) =>
  Mapping (FnMapFromRep a) rep a where
  mapping _ x = to x

---

class MkMatcher :: Type -> Row Type -> Type -> Constraint
class MkMatcher a r z | a -> r where
  mkMatcher :: Fn2 a (Record r) z

instance
  ( Generic a rep
  , MkMatcherRep rep () r z
  ) =>
  MkMatcher a r z where
  mkMatcher = mkFn2 \val rec -> mkMatcherRep @_ @() @_ @z rec (from val)

class MkMatcher1 :: (Type -> Type) -> Type -> Type -> Row Type -> Constraint
class MkMatcher1 fa a z r | fa a -> r where
  mkMatcher1 :: Fn2 (fa a) (Record r) z

instance
  ( Generic (f a) rep
  , MkMatcherRep rep () r z
  ) =>
  MkMatcher1 f a z r where
  mkMatcher1 = mkFn2 \val rec -> mkMatcherRep @rep @() @r @z rec (from val)

class MkMatcherRep :: Type -> Row Type -> Row Type -> Type -> Constraint
class MkMatcherRep rep rin r z | rep rin -> r where
  mkMatcherRep :: Record r -> rep -> z

instance (MkMatcherRep a rin ra z, MkMatcherRep b rin rb z, Union ra rb r) => MkMatcherRep (Sum a b) rin r z where
  mkMatcherRep rec val = ret
    where
    ret :: z
    ret = case val of
      Inl x -> mkMatcherRep @a @rin @ra @z (dropFields rec :: Record ra) x
      Inr x -> mkMatcherRep @b @rin @rb @z (dropFields' rec :: Record rb) x

dropFields :: forall rx r2 r1. (Row.Union r2 rx r1) => Record r1 -> Record r2
dropFields = unsafeCoerce

dropFields' :: forall rx r2 r1. (Row.Union rx r2 r1) => Record r1 -> Record r2
dropFields' = unsafeCoerce

instance
  ( Row.Cons sym (Unit -> z) rin r
  , IsSymbol sym
  ) =>
  MkMatcherRep (Constructor sym NoArguments) rin r z where
  mkMatcherRep rec _ = ret
    where
    ret :: z
    ret = on unit

    on :: Unit -> z
    on = Record.get (Proxy :: _ sym) rec

instance
  ( Row.Cons sym (arg -> z) rin r
  , IsSymbol sym
  ) =>
  MkMatcherRep (Constructor sym (Argument arg)) rin r z where
  mkMatcherRep rec (Constructor (Argument arg)) = ret
    where
    ret :: z
    ret = on arg

    on :: arg -> z
    on = Record.get (Proxy :: _ sym) rec
