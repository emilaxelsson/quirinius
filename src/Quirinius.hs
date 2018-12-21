{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

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

module Quirinius where

import Data.Function ((&))
import qualified Data.List as List
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import GHC.Types (Constraint)
import GHC.Records (HasField (..))
import Type.Reflection (Typeable, TypeRep, typeRep)



--------------------------------------------------------------------------------
-- * Type constraints for promoted lists
--------------------------------------------------------------------------------

-- | A witness that each type in a list fulfills a given predicate
data WitnessEach (p :: k -> Constraint) (as :: [k]) where
  WNil :: WitnessEach p '[]
  WCons :: p a => WitnessEach p as -> WitnessEach p (a ': as)

-- | All types fulfill the given predicate
class All (p :: k -> Constraint) (as :: [k]) where
  witnessEach :: WitnessEach p as

instance All p '[] where
  witnessEach = WNil

instance (p a, All p as) => All p (a ': as) where
  witnessEach = WCons (witnessEach :: WitnessEach p as)



--------------------------------------------------------------------------------
-- * Anonymous records
--------------------------------------------------------------------------------

-- | A named field
data f :-> a where
  Field :: KnownSymbol f => a -> f :-> a

deriving instance Eq a => Eq (f :-> a)

instance Show a => Show (f :-> a) where
  show (Field a :: f :-> a) = show (symbolVal (Proxy @f)) ++ " -> " ++ show a

infixr 6 :->

listFields :: forall fs. All KnownSymbol fs => [Text]
listFields = go (witnessEach :: WitnessEach KnownSymbol fs)
  where
    go :: WitnessEach KnownSymbol gs -> [Text]
    go WNil = []
    go w@(WCons ws) = showCons w : go ws

    showCons :: forall g gs'. WitnessEach KnownSymbol (g ': gs') -> Text
    showCons (WCons _) = Text.pack (symbolVal (Proxy @g))
      -- This helper function is needed in order to pattern match on the type
      -- `g ': gs'`.

-- | Anonymous record
data Record fs where
  RNil :: Record '[]
  RCons :: f :-> a -> Record fs -> Record (f :-> a ': fs)

instance All Show fs => Show (Record fs) where
  show = List.intercalate ", " . go (witnessEach :: WitnessEach Show fs)
    where
      go :: WitnessEach Show fs' -> Record fs' -> [Prelude.String]
      go WNil RNil = []
      go (WCons w) (RCons f fs) = show f : go w fs

-- | Pick first matching field
instance {-# OVERLAPPING #-} (a1 ~ a2) =>
                             HasField f (Record (f :-> a1 : fs)) a2 where
  getField (RCons (Field a) _) = a

instance {-# OVERLAPPABLE #-} (HasField f (Record fs) a) =>
                              HasField (f :: Symbol) (Record (f' ': fs)) a where
  getField (RCons _ fs) = getField @f fs

-- | Get the field names of a list of ':->' fields
type family FieldNames fs where
  FieldNames '[] = '[]
  FieldNames (f :-> a ': fs) = f ': FieldNames fs

class (ns ~ FieldNames fs) => Sliceable r ns fs | r ns -> fs where
  slice' :: r -> proxy ns -> Record fs

instance Sliceable r '[] '[] where
  slice' _ _ = RNil

instance (HasField n r a, KnownSymbol n, Sliceable r ns fs, n ~ f) =>
         Sliceable r (n ': ns) (f :-> a ': fs) where
  slice' r _ = RCons (Field $ getField @n r) (slice' r (Proxy @ns))

-- | Extract a subset of the fields of a record, return as an anonymous 'Record'
slice :: forall ns fs r. Sliceable r ns fs => r -> Record fs
slice r = slice' r (Proxy @ns)



--------------------------------------------------------------------------------
-- * Queries
--------------------------------------------------------------------------------

-- | Query expressions
data QExp tab a where
  Lit :: Show a => a -> QExp tab a
  Fld :: (HasField f tab a, KnownSymbol f) => proxy f -> QExp tab a
  Eq :: Eq a => QExp tab a -> QExp tab a -> QExp tab Bool
  And :: QExp tab Bool -> QExp tab Bool -> QExp tab Bool
  Or :: QExp tab Bool -> QExp tab Bool -> QExp tab Bool
  Add :: Num a => QExp tab a -> QExp tab a -> QExp tab a
  Sub :: Num a => QExp tab a -> QExp tab a -> QExp tab a
  Mul :: Num a => QExp tab a -> QExp tab a -> QExp tab a
  Inner :: Typeable inner => Query inner a -> QExp tab a

instance (Num a, Show a) => Num (QExp tab a) where
  fromInteger = Lit . fromInteger
  (+) = Add
  (-) = Sub
  (*) = Mul
  abs = undefined
  signum = undefined

-- | A query that returns rows of a table
data RowQuery tab a where
  Table :: RowQuery tab tab
  Select
    :: (All KnownSymbol ns, Sliceable r ns fs)
    => proxy ns
    -> RowQuery tab r
    -> RowQuery tab (Record fs)
  Where :: QExp tab Bool -> RowQuery tab a -> RowQuery tab a

-- | A general query
data Query tab a where
  Rows  :: RowQuery tab a -> Query tab a
  Count :: RowQuery tab a -> Query tab Integer
  Sum   :: Num a => QExp tab a -> RowQuery tab b -> Query tab a

-- | Types that represent queries
class ToQuery q where
  toQuery :: q tab a -> Query tab a

instance ToQuery RowQuery where
  toQuery = Rows

instance ToQuery Query where
  toQuery = id

withRowQuery :: Query tab a -> (forall b. RowQuery tab b -> c) -> c
withRowQuery (Rows q)  k = k q
withRowQuery (Count q) k = k q
withRowQuery (Sum _ q) k = k q

-- | Query a table in the database
table :: RowQuery tab tab
table = Table

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
  -> RowQuery tab (Record fs)
select = Select (Proxy @ns)

-- | Restrict a query to only return a subset of the rows
--
-- The rows to return are those that fulfill the given predicate.
where_ ::
     QExp tab Bool -- ^ Predicate
  -> RowQuery tab a
  -> RowQuery tab a
where_ = Where

-- | Count the number of rows
count :: RowQuery tab a -> Query tab Integer
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

(#=) :: Eq a => QExp tab a -> QExp tab a -> QExp tab Bool
(#=) = Eq

-- | Lift an inner query to an expression
inner :: (ToQuery q, Typeable inner) => q inner a -> QExp tab a
inner = Inner . toQuery



----------------------------------------
-- ** Compilation
----------------------------------------

compileQExp :: QExp tab a -> Text
compileQExp (Lit a)   = Text.pack $ show a
compileQExp (Fld p)   = Text.pack $ symbolVal p
compileQExp (Eq a b)  = compileQExp a <> " = "   <> compileQExp b
compileQExp (And a b) = compileQExp a <> " AND " <> compileQExp b
compileQExp (Or a b)  = compileQExp a <> " OR "  <> compileQExp b
compileQExp (Add a b) = compileQExp a <> " + "   <> compileQExp b
compileQExp (Sub a b) = compileQExp a <> " - "   <> compileQExp b
compileQExp (Mul a b) = compileQExp a <> " * "   <> compileQExp b
compileQExp (Inner q) = "(" <> compileQuery q <> ")"

extractSelection :: RowQuery tab a -> Maybe [Text]
extractSelection Table = Nothing
extractSelection (Select (_ :: proxy ns) q) =
  Just $
  case extractSelection q of
    Nothing -> ns
    Just ns' -> List.intersect ns ns'
  where
    ns = listFields @ns
extractSelection (Where _ q) = extractSelection q

extractWhere :: RowQuery tab a -> QExp tab Bool
extractWhere Table = Lit True
extractWhere (Select _ q) = extractWhere q
extractWhere (Where w q) =
  case extractWhere q of
    Lit True -> w
    w' -> And w w'

compileSelection :: Maybe [Text] -> Text
compileSelection Nothing = "*"
compileSelection (Just fs) = Text.intercalate ", " fs

compileResult :: Query tab a -> Text
compileResult (Rows q)  = compileSelection $ extractSelection q
compileResult (Count _) = "count(*)"
compileResult (Sum a _) = "sum(" <> compileQExp a <> ")"

compileQuery' :: forall tab a. Typeable tab => Query tab a -> Text
compileQuery' q = Text.unwords $ concat
  [ [ "SELECT"
    , compileResult q
    , "FROM"
    , Text.pack $ show (typeRep :: TypeRep tab)
    ]
  , case withRowQuery q extractWhere of
      Lit True -> []
      w -> ["WHERE", compileQExp w]
  ]

compileQuery :: (ToQuery q, Typeable tab) => q tab a -> Text
compileQuery = compileQuery' . toQuery



--------------------------------------------------------------------------------
-- * Testing
--------------------------------------------------------------------------------

data Person = Person
  { name :: Text
  , age :: Int
  , height :: Double
  }

data Car = Car
  { brand :: Text
  , speed :: Double
  }

-- Selection
q1 =
  table @Person &
  where_ (fld @"age" #= 3) &
  where_ (fld @"height" #= 3) &
  select @'["name"]

-- Summation
q2 =
  table @Person &
  where_ (fld @"age" #= 3) &
  where_ (fld @"height" #= 3) &
  sum_ (fld @"height" * 2)

-- Inner query
q3 =
  table @Car &
  where_ (fld @"speed" #= inner q2) &
  select @'["brand"]

test = do
  Text.putStrLn $ compileQuery q1
  Text.putStrLn $ compileQuery q2
  Text.putStrLn $ compileQuery q3
