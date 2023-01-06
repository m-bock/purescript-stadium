module RecordLike where

import Prelude

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Product(..), from, to)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Int as Int
import Prim.Row (class Union)
import Prim.Row as Row
import Record as Rec
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class RecordLike a r | a -> r where
  toRecord :: a -> Record r
  fromRecord :: Record r -> a

instance (GenericRecordLike (Tuple a b) r) => RecordLike (Tuple a b) r where
  toRecord = genericToRecord
  fromRecord = genericFromRecord

instance RecordLike (Record r) r where
  toRecord = identity
  fromRecord = identity

---

class GenericRecordLike a r where
  genericToRecord :: a -> Record r
  genericFromRecord :: Record r -> a

instance (Generic a rep, RepRecordLike 0 rep r) => GenericRecordLike a r where
  genericToRecord = repToRecord (Proxy :: _ 0) <<< from
  genericFromRecord = to <<< repFromRecord (Proxy :: _ 0)

---

class RepRecordLike (ix :: Int) rep r | rep -> r where
  repToRecord :: Proxy ix -> rep -> Record r
  repFromRecord :: Proxy ix -> Record r -> rep

instance (RepRecordLike ix args r) => RepRecordLike ix (Constructor s args) r
  where
  repToRecord ix (Constructor args) =
    repToRecord ix args

  repFromRecord ix r =
    Constructor $ repFromRecord ix r

instance RepRecordLike ix NoArguments ()
  where
  repToRecord _ _ = {}
  repFromRecord _ _ = NoArguments

instance
  ( RepRecordLike ix a r1
  , RepRecordLike ix' b r2
  , Int.Add ix 1 ix'
  , Union r1 r2 r
  , Union r2 r1 r
  , Int.ToString ix ixs
  ) =>
  RepRecordLike ix (Product a b) r
  where
  repToRecord _ (Product a b) = Rec.union a' b'
    where
    a' = repToRecord (Proxy :: _ ix) a
    b' = repToRecord (Proxy :: _ ix') b

  repFromRecord _ r = Product a b
    where
    a = pick r # repFromRecord (Proxy :: _ ix)
    b = pick r # repFromRecord (Proxy :: _ ix')

instance
  ( Row.Cons ixs a () r
  , Int.ToString ix ixs
  , IsSymbol ixs
  ) =>
  RepRecordLike ix (Argument a) r
  where
  repToRecord _ (Argument x) =
    Rec.insert (Proxy :: _ ixs) x {}

  repFromRecord _ r =
    Argument $ Rec.get (Proxy :: _ ixs) r

--- Internal Util

pick :: forall r2 rx r1. Union r2 rx r1 => { | r1 } -> { | r2 }
pick = unsafeCoerce

--- Tests

t1
  :: { "0" :: Int
     , "1" :: String
     }
t1 = toRecord $ Foo 3 ""

t2 :: Foo
t2 = fromRecord { "0": 1, "1": "" }

t3
  :: { "0" :: Int
     , "1" :: String
     }
t3 = toRecord (Tuple 1 "s")

---

data Foo = Foo Int String

derive instance Generic Foo _

instance GenericRecordLike Foo r => RecordLike Foo r where
  toRecord = genericToRecord
  fromRecord = genericFromRecord
