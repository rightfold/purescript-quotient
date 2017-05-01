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
  , Mod
  ) where

import Data.Ord (abs)
import Prelude
import Type.Nat1 (kind Nat1, class IsNat1, N1Proxy(..), reifyNat1)
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
  show a = "(mkQuotient " <> show a <> ")"

--------------------------------------------------------------------------------

-- | `T / Id ~ T`.
foreign import data Id :: Type

-- | Negative values are equivalent to their positive counterparts.
foreign import data Abs :: Type

-- | Integers modulo some positive number.
foreign import data Mod :: Nat1 -> Type

instance canonicalId :: Canonical a Id where
  canonical _ = id

instance canonicalAbs :: (Ord a, Ring a) => Canonical a Abs where
  canonical _ = abs

instance canonicalMod :: (IsNat1 n) => Canonical Int (Mod n) where
  canonical _ a = a `mod` reifyNat1 (N1Proxy :: N1Proxy n)
