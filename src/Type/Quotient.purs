-- | Approximation of quotient types.
module Type.Quotient
  ( class Canonical
  , canonical

  , Quotient
  , type (/)
  , mkQuotient
  , runQuotient
  ) where

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
mkQuotient :: ∀ a e. a -> Quotient a e
mkQuotient = Quotient

-- | Canonicalize a value using an equivalence relation such that the caller
-- | cannot observe distinct wrappees.
runQuotient :: ∀ a e. Canonical a e => Quotient a e -> a
runQuotient (Quotient a) = canonical (Proxy :: Proxy e) a
