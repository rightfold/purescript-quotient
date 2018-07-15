-- | Approximation of quotient types.
module Type.Quotient
  ( class Canonical
  , canonical

  , Quotient
  , type (/)
  , mkQuotient
  , runQuotient

  , Id
  , Abs
  , Mod2
  , Mod4
  , Mod8
  , Mod16
  , Mod32
  , Mod64
  , Mod128
  , Mod256
  , Mod512
  , Mod1024
  ) where

import Data.Ord (abs)
import Prelude
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary, coarbitrary)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

-- | Equivalence relation disguised as a canonicalization function.
class Canonical a e | e -> a where
  canonical :: Proxy e -> a -> a

--------------------------------------------------------------------------------

-- | Quotient type with equivalence relation `e`. The runtime representation is
-- | identical to that of `a`.
newtype Quotient a e = Quotient a

infixl 9 type Quotient as /

-- | Pair a value with an equivalence relation.
mkQuotient :: ∀ a e. a -> a / e
mkQuotient = Quotient

-- | Canonicalize a value using an equivalence relation such that the caller
-- | cannot observe distinct wrappees.
runQuotient :: ∀ a e. Canonical a e => a / e -> a
runQuotient (Quotient a) = canonical (Proxy :: Proxy e) a

instance eqQuotient :: (Eq a, Canonical a e) => Eq (a / e) where
  eq a b = eq (runQuotient a) (runQuotient b)

instance ordQuotient :: (Ord a, Canonical a e) => Ord (a / e) where
  compare a b = compare (runQuotient a) (runQuotient b)

instance showQuotient :: (Show a, Canonical a e) => Show (a / e) where
  show a = "(mkQuotient " <> show (runQuotient a) <> ")"

instance arbitraryQuotient :: (Arbitrary a, Canonical a e) => Arbitrary (a / e) where
  arbitrary = mkQuotient <$> arbitrary

instance coarbitraryQuotient :: (Coarbitrary a, Canonical a e) => Coarbitrary (a / e) where
  coarbitrary = coarbitrary <<< runQuotient

--------------------------------------------------------------------------------

-- | `T / Id ~ T`.
foreign import data Id :: Type

-- | Negative values are equivalent to their positive counterparts.
foreign import data Abs :: Type

-- | Non-negative integers modulo 2.
foreign import data Mod2 :: Type

-- | Non-negative integers modulo 4.
foreign import data Mod4 :: Type

-- | Non-negative integers modulo 8.
foreign import data Mod8 :: Type

-- | Non-negative integers modulo 16.
foreign import data Mod16 :: Type

-- | Non-negative integers modulo 32.
foreign import data Mod32 :: Type

-- | Non-negative integers modulo 64.
foreign import data Mod64 :: Type

-- | Non-negative integers modulo 128.
foreign import data Mod128 :: Type

-- | Non-negative integers modulo 256.
foreign import data Mod256 :: Type

-- | Non-negative integers modulo 512.
foreign import data Mod512 :: Type

-- | Non-negative integers modulo 1024.
foreign import data Mod1024 :: Type

instance canonicalId :: Canonical a Id where
  canonical _ = identity

instance canonicalAbs :: (Ord a, Ring a) => Canonical a Abs where
  canonical _ = abs

instance canonicalMod2 :: Canonical Int Mod2 where
  canonical _ = abs <<< (_ `mod` 2)

instance canonicalMod4 :: Canonical Int Mod4 where
  canonical _ = abs <<< (_ `mod` 4)

instance canonicalMod8 :: Canonical Int Mod8 where
  canonical _ = abs <<< (_ `mod` 8)

instance canonicalMod16 :: Canonical Int Mod16 where
  canonical _ = abs <<< (_ `mod` 16)

instance canonicalMod32 :: Canonical Int Mod32 where
  canonical _ = abs <<< (_ `mod` 32)

instance canonicalMod64 :: Canonical Int Mod64 where
  canonical _ = abs <<< (_ `mod` 64)

instance canonicalMod128 :: Canonical Int Mod128 where
  canonical _ = abs <<< (_ `mod` 128)

instance canonicalMod256 :: Canonical Int Mod256 where
  canonical _ = abs <<< (_ `mod` 256)

instance canonicalMod512 :: Canonical Int Mod512 where
  canonical _ = abs <<< (_ `mod` 512)

instance canonicalMod1024 :: Canonical Int Mod1024 where
  canonical _ = abs <<< (_ `mod` 1024)
