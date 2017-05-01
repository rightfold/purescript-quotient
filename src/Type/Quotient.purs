-- | Approximation of quotient types.
module Type.Quotient
  ( class Canonical
  , canonical

  , Quotient
  , type (/)
  , mkQuotient
  , runQuotient
  ) where

import Prelude
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
