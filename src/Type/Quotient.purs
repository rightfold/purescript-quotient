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

class Canonical a e | e -> a where
  canonical :: Proxy e -> a -> a

--------------------------------------------------------------------------------

newtype Quotient a e = Quotient a

infixl 9 type Quotient as /

mkQuotient :: ∀ a e. a -> Quotient a e
mkQuotient = Quotient

runQuotient :: ∀ a e. Canonical a e => Quotient a e -> a
runQuotient (Quotient a) = canonical (Proxy :: Proxy e) a
