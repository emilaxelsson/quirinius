{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Anonymous records

module Quirinius.Record.Anonymous where

import qualified Data.List as List
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import GHC.Types (Constraint)
import GHC.Records (HasField (..))



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
  show (Field a :: f :-> a) = symbolVal (Proxy @f) ++ " = " ++ show a

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
  show =
    ("{" ++) .
    (++ "}") . List.intercalate ", " . go (witnessEach :: WitnessEach Show fs)
    -- TODO Use `shows`.
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

-- | Extract the value of a single-field record
fromSingle :: Record '[f :-> a] -> a
fromSingle (RCons (Field a) RNil) = a

-- | Get the field names of a type-level list of fields
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
