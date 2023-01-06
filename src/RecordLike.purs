module RecordLike where

import Prelude

import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments, Product(..), from)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Prim.Int as Int
import Prim.Row (class Union)
import Prim.Row as Row
import Record as Rec
import Type.Proxy (Proxy(..))

class RecordLike a r where
  toRecord :: a -> Record r

instance (GenericToRecord (Tuple a b) r) => RecordLike (Tuple a b) r where
  toRecord = genericToRecord

instance RecordLike (Record r) r where
  toRecord = identity

---

class GenericToRecord a r where
  genericToRecord :: a -> Record r

instance (Generic a rep, RepToRecord rep r 0) => GenericToRecord a r where
  genericToRecord = repToRecord (Proxy :: _ 0) <<< from

---

class RepToRecord rep r (ix :: Int) | rep -> r where
  repToRecord :: Proxy ix -> rep -> Record r

instance (RepToRecord args r ix) => RepToRecord (Constructor s args) r ix
  where
  repToRecord ix (Constructor args) = repToRecord ix args

instance RepToRecord NoArguments () ix
  where
  repToRecord _ _ = {}

instance
  ( RepToRecord a r1 ix
  , RepToRecord b r2 ix'
  , Int.Add ix 1 ix'
  , Union r1 r2 r
  ) =>
  RepToRecord (Product a b) r ix
  where
  repToRecord _ (Product a b) =
    repToRecord (Proxy :: _ ix) a
      `Rec.union`
        repToRecord (Proxy :: _ ix') b

instance
  ( Row.Cons ixs a () r
  , Int.ToString ix ixs
  , IsSymbol ixs
  ) =>
  RepToRecord (Argument a) r ix
  where
  repToRecord _ (Argument x) = {} # Rec.insert (Proxy :: _ ixs) x

---

x2
  :: { "0" :: Int
     , "1" :: String
     }
x2 = toRecord $ Foo 3 ""

x
  :: { "0" :: Int
     , "1" :: String
     }
x = toRecord (Tuple 1 "s")

---

data Foo = Foo Int String

derive instance Generic Foo _

instance GenericToRecord Foo r => RecordLike Foo r where
  toRecord = genericToRecord