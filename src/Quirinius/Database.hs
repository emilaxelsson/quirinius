{-# LANGUAGE UndecidableInstances #-}

-- | Database interface for Haskell records, based on the @persistent@ package

module Quirinius.Database where

import Data.Char (toLower)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics
  ( (:*:)(..)
  , C1
  , D1
  , Generic
  , K1(..)
  , M1(..)
  , Meta(..)
  , Rec0
  , Rep
  , S1
  )
import qualified GHC.Generics as Generics
import GHC.TypeLits (KnownSymbol, symbolVal)

import Database.Persist.Sql (PersistField(..), RawSql, Single(..))

import Quirinius.Record.Anonymous

class GTableRecord a where
  gTableName :: proxy a -> Text

instance KnownSymbol name => GTableRecord (D1 ('MetaData name x y z) r) where
  gTableName _ = Text.pack $ map toLower $ symbolVal (Proxy @name)

-- | Records that correspond to database tables
--
-- Ordinary records can use the generic instance; for example:
--
-- > data Person = Person
-- >   { name :: Text
-- >   , age  :: Int
-- >   } deriving Generic
-- >
-- > instance TableRecord Person
class TableRecord a where
  tableName :: proxy a -> Text

  default tableName :: GTableRecord (Rep a) => proxy a -> Text
  tableName _ = gTableName (Proxy @(Rep a))

class RawSql (GRawSqlRep rep) => GQueryResult rep where
  type GRawSqlRep (rep :: * -> *)
  gSerializeResult   :: rep x -> GRawSqlRep rep
  gDeserializeResult :: GRawSqlRep rep -> rep x

instance GQueryResult rep => GQueryResult (D1 meta rep) where
  type GRawSqlRep (D1 meta rep) = GRawSqlRep rep
  gSerializeResult   = gSerializeResult . unM1
  gDeserializeResult = M1 . gDeserializeResult

instance GQueryResult rep => GQueryResult (C1 meta rep) where
  type GRawSqlRep (C1 meta rep) = GRawSqlRep rep
  gSerializeResult   = gSerializeResult . unM1
  gDeserializeResult = M1 . gDeserializeResult

instance PersistField a =>
         GQueryResult (S1 ('MetaSel ('Just name) x y z) (Rec0 a)) where
  type GRawSqlRep (S1 ('MetaSel ('Just name) x y z) (Rec0 a)) = Single a
  gSerializeResult   = Single . unK1 . unM1
  gDeserializeResult = M1 . K1 . unSingle

instance (GQueryResult rep1, GQueryResult rep2) =>
         GQueryResult (rep1 :*: rep2) where
  type GRawSqlRep (rep1 :*: rep2) = (GRawSqlRep rep1, GRawSqlRep rep2)
  gSerializeResult (rep1 :*: rep2) =
    (gSerializeResult rep1, gSerializeResult rep2)
  gDeserializeResult (a, b) = gDeserializeResult a :*: gDeserializeResult b

-- | Types that can be returned from queries
--
-- Ordinary records can use the generic instance; for example:
--
-- > data Person = Person
-- >   { name :: Text
-- >   , age  :: Int
-- >   } deriving Generic
-- >
-- > instance QueryResult Person
class RawSql (RawSqlRep a) => QueryResult a where
  -- | Representation of the record as a type that's a member of 'RawSql'
  type RawSqlRep a
  type instance RawSqlRep a = GRawSqlRep (Rep a)

  -- | Serialize a result
  serializeResult :: a -> RawSqlRep a

  -- | Deserialize a result
  deserializeResult :: RawSqlRep a -> a

  default serializeResult ::
       (Generic a, GQueryResult (Rep a), RawSqlRep a ~ GRawSqlRep (Rep a))
    => a
    -> RawSqlRep a
  serializeResult = gSerializeResult . Generics.from

  default deserializeResult ::
       (Generic a, GQueryResult (Rep a), RawSqlRep a ~ GRawSqlRep (Rep a))
    => RawSqlRep a
    -> a
  deserializeResult = Generics.to . gDeserializeResult

instance QueryResult String where
  type RawSqlRep String = Single String
  serializeResult   = Single
  deserializeResult = unSingle

instance QueryResult Text where
  type RawSqlRep Text = Single Text
  serializeResult   = Single
  deserializeResult = unSingle

instance QueryResult Int where
  type RawSqlRep Int = Single Int
  serializeResult   = Single
  deserializeResult = unSingle

instance QueryResult Double where
  type RawSqlRep Double = Single Double
  serializeResult   = Single
  deserializeResult = unSingle

instance (KnownSymbol f, PersistField a) =>
         QueryResult (Record '[f :-> a]) where
  type RawSqlRep (Record '[f :-> a]) = Single a
  serializeResult   (RCons (Field a) RNil) = Single a
  deserializeResult (Single a)             = RCons (Field a) RNil

instance (KnownSymbol f, PersistField a, QueryResult (Record (f' ': fs))) =>
         QueryResult (Record (f :-> a ': f' ': fs)) where
  type RawSqlRep (Record (f :-> a ': f' ': fs)) = ( Single a
                                                  , RawSqlRep (Record (f' ': fs)))
  serializeResult (RCons (Field a) r) = (Single a, serializeResult r)
  deserializeResult (Single a, r)     = RCons (Field a) (deserializeResult r)
