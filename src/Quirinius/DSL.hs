{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- Quirinius is a simple query EDSL inspired by Rails active record queries.
--
--   * Queries are built by stacking operations using `&`.
--   * Type-safety is obtained by relying on Haskell's `HasField` class.
--   * Inner queries are supported.
--
-- At the moment, it's only possible to compile queries to SQL. But it should be
-- possible to also run the queries and get back Haskell values of the type
-- specified by the query.
--
-- Note the absence of TemplateHaskell or even generics!
--
-- See the examples at the bottom to get a feeling for how the EDSL works.

module Quirinius.DSL where

import Prelude hiding (GT, LT)

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, symbolVal)

import Quirinius.Database
import Quirinius.Record.Anonymous



--------------------------------------------------------------------------------
-- * Queries
--------------------------------------------------------------------------------

-- | Query expressions
data QExp tab a where
  Lit   :: Show a => a -> QExp tab a
  Fld   :: (HasField f tab a, KnownSymbol f) => proxy f -> QExp tab a
  MFld  :: (HasField f tab (Maybe a), KnownSymbol f) => proxy f -> QExp tab a
  Eq    :: Eq a => QExp tab a -> QExp tab a -> QExp tab Bool
  NEq   :: Eq a => QExp tab a -> QExp tab a -> QExp tab Bool
  GT    :: Ord a => QExp tab a -> QExp tab a -> QExp tab Bool
  GTE   :: Ord a => QExp tab a -> QExp tab a -> QExp tab Bool
  LT    :: Ord a => QExp tab a -> QExp tab a -> QExp tab Bool
  LTE   :: Ord a => QExp tab a -> QExp tab a -> QExp tab Bool
  And   :: QExp tab Bool -> QExp tab Bool -> QExp tab Bool
  Or    :: QExp tab Bool -> QExp tab Bool -> QExp tab Bool
  Add   :: Num a => QExp tab a -> QExp tab a -> QExp tab a
  Sub   :: Num a => QExp tab a -> QExp tab a -> QExp tab a
  Mul   :: Num a => QExp tab a -> QExp tab a -> QExp tab a
  Inner :: TableRecord inner => Query inner a -> QExp tab a

instance (Num a, Show a) => Num (QExp tab a) where
  fromInteger = Lit . fromInteger
  (+) = Add
  (-) = Sub
  (*) = Mul
  abs = undefined
  signum = undefined

instance (Fractional a, Show a) => Fractional (QExp tab a) where
  fromRational = Lit . fromRational
  (/) = undefined -- TODO

-- | A query that returns rows of a table
data RowQuery tab a where
  From  :: RowQuery tab tab
  Where :: QExp tab Bool -> RowQuery tab a -> RowQuery tab a
  -- The second type parameter is just to get the same kind as `Query`.

-- | A general query
data Query tab a where
  Table   :: RowQuery tab a -> Query tab a
  Project :: QExp tab b -> RowQuery tab a -> Query tab b
  Select  :: (All KnownSymbol ns, Sliceable r ns fs)
          => proxy ns
          -> RowQuery tab r
          -> Query tab (Record fs)
  Count   :: RowQuery tab a -> Query tab Int
  Sum     :: Num a => QExp tab a -> RowQuery tab b -> Query tab a

-- | Types that represent queries
class ToQuery q where
  toQuery :: q tab a -> Query tab a

instance ToQuery RowQuery where
  toQuery q = Table q

instance ToQuery Query where
  toQuery = id

withRowQuery :: Query tab a -> (forall b. RowQuery tab b -> c) -> c
withRowQuery (Table q)     k = k q
withRowQuery (Project _ q) k = k q
withRowQuery (Select _ q)  k = k q
withRowQuery (Count q)     k = k q
withRowQuery (Sum _ q)     k = k q

-- | Query a table in the database
table :: RowQuery tab tab
table = From

-- | Restrict a query to only return a subset of the rows
--
-- The rows to return are those that fulfill the given predicate.
where_ ::
     QExp tab Bool -- ^ Predicate
  -> RowQuery tab a
  -> RowQuery tab a
where_ = Where

-- | Project a value from a query
project :: QExp tab a -> RowQuery tab r -> Query tab a
project = Project

-- | Restrict a query to only return a subset of the columns
--
-- The columns to return are specified through the first type parameter, e.g.
-- like so:
--
-- @
-- `select` @'["name", "age"]
-- @
select ::
     forall ns fs tab r. (All KnownSymbol ns, Sliceable r ns fs)
  => RowQuery tab r
  -> Query tab (Record fs)
select = Select (Proxy @ns)

-- | Count the number of rows
count :: RowQuery tab a -> Query tab Int
count = Count

-- | Sum a given projection of the rows
sum_ ::
     Num a
  => QExp tab a -- ^ Projection
  -> RowQuery tab b
  -> Query tab a
sum_ = Sum

-- | Get the value of a field (i.e. column)
fld :: forall f tab a. (HasField f tab a, KnownSymbol f) => QExp tab a
fld = Fld (Proxy @f)

-- | Get the value of a partial field (i.e. column)
mfld :: forall f tab a. (HasField f tab (Maybe a), KnownSymbol f) => QExp tab a
mfld = MFld (Proxy @f)

class ColumnType a b | a -> b where
  -- | Get the value of a field (i.e. column)
  --
  -- This function generalized 'fld' and 'mfld'.
  getFld :: (HasField f tab a, KnownSymbol f) => proxy f -> QExp tab b

instance {-# OVERLAPPABLE #-} (a ~ b) => ColumnType a b where
  getFld = Fld

instance {-# OVERLAPPING #-} ColumnType (Maybe a) a where
  getFld = MFld

instance (HasField f tab a, KnownSymbol f, ColumnType a b) =>
         IsLabel f (QExp tab b) where
  fromLabel = getFld (Proxy @f)

(.=) :: Eq a => QExp tab a -> QExp tab a -> QExp tab Bool
(.=) = Eq

(./=) :: Eq a => QExp tab a -> QExp tab a -> QExp tab Bool
(./=) = NEq

(.>) :: Ord a => QExp tab a -> QExp tab a -> QExp tab Bool
(.>) = GT

(.>=) :: Ord a => QExp tab a -> QExp tab a -> QExp tab Bool
(.>=) = GTE

(.<) :: Ord a => QExp tab a -> QExp tab a -> QExp tab Bool
(.<) = LT

(.<=) :: Ord a => QExp tab a -> QExp tab a -> QExp tab Bool
(.<=) = LTE

-- | Get the result of an inner query
inner :: (ToQuery q, TableRecord inner) => q inner a -> QExp tab a
inner = Inner . toQuery



--------------------------------------------------------------------------------
-- * SQL compilation
--------------------------------------------------------------------------------

compileQExp :: QExp tab a -> Text
compileQExp (Lit a)   = Text.pack $ show a
compileQExp (Fld p)   = Text.pack $ symbolVal p
compileQExp (MFld p)  = Text.pack $ symbolVal p
compileQExp (Eq a b)  = compileQExp a <> " = "   <> compileQExp b
compileQExp (NEq a b) = compileQExp a <> " <> "  <> compileQExp b
compileQExp (GT a b)  = compileQExp a <> " > "   <> compileQExp b
compileQExp (GTE a b) = compileQExp a <> " >= "  <> compileQExp b
compileQExp (LT a b)  = compileQExp a <> " < "   <> compileQExp b
compileQExp (LTE a b) = compileQExp a <> " <= "  <> compileQExp b
compileQExp (And a b) = compileQExp a <> " AND " <> compileQExp b
compileQExp (Or a b)  = compileQExp a <> " OR "  <> compileQExp b
compileQExp (Add a b) = compileQExp a <> " + "   <> compileQExp b
compileQExp (Sub a b) = compileQExp a <> " - "   <> compileQExp b
compileQExp (Mul a b) = compileQExp a <> " * "   <> compileQExp b
compileQExp (Inner q) = "(" <> compileQuery q <> ")"

extractWhere :: RowQuery tab a -> QExp tab Bool
extractWhere From = Lit True
extractWhere (Where w q) =
  case extractWhere q of
    Lit True -> w
    w' -> And w w'

compileResult :: Query tab a -> Text
compileResult (Table _)                 = "*"
compileResult (Project proj _)          = compileQExp proj
compileResult (Select  (_ :: prx ns) _) = Text.intercalate ", " $ listFields @ns
compileResult (Count _)                 = "count(*)"
compileResult (Sum a _)                 = "sum(" <> compileQExp a <> ")"

compileQuery' :: forall tab a. TableRecord tab => Query tab a -> Text
compileQuery' q = Text.unwords $ concat
  [ [ "SELECT"
    , compileResult q
    , "FROM"
    , tableName (Proxy @tab)
    ]
  , case withRowQuery q extractWhere of
      Lit True -> []
      w -> ["WHERE", compileQExp w]
  ]

compileQuery :: (ToQuery q, TableRecord tab) => q tab a -> Text
compileQuery = compileQuery' . toQuery
