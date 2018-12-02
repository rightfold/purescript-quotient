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
  , Mod2048
  , Mod4096
  , Mod8192
  , Mod16384
  , Mod32768
  , Mod65536
  , Mod131072
  , Mod262144
  , Mod524288
  , Mod1048576
  , Mod2097152
  , Mod4194304
  , Mod8388608
  , Mod16777216
  , Mod33554432
  , Mod67108864
  , Mod134217728
  , Mod268435456
  , Mod536870912
  , Mod1073741824
  , Mod2147483648
  , Mod4294967296
  , Byte
  , Nibble
  , Word
  ) where

import Prelude

import Data.Ord (abs)
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

-- | Non-negative integers modulo 2048.
foreign import data Mod2048 :: Type

-- | Non-negative integers modulo 4096.
foreign import data Mod4096 :: Type

-- | Non-negative integers modulo 8192.
foreign import data Mod8192 :: Type

-- | Non-negative integers modulo 16384.
foreign import data Mod16384 :: Type

-- | Non-negative integers modulo 32768.
foreign import data Mod32768 :: Type

-- | Non-negative integers modulo 65536.
foreign import data Mod65536 :: Type

-- | Non-negative integers modulo 131072.
foreign import data Mod131072 :: Type

-- | Non-negative integers modulo 262144.
foreign import data Mod262144 :: Type

-- | Non-negative integers modulo 524288.
foreign import data Mod524288 :: Type

-- | Non-negative integers modulo 1048576.
foreign import data Mod1048576 :: Type

-- | Non-negative integers modulo 2097152.
foreign import data Mod2097152 :: Type

-- | Non-negative integers modulo 4194304.
foreign import data Mod4194304 :: Type

-- | Non-negative integers modulo 8388608.
foreign import data Mod8388608 :: Type

-- | Non-negative integers modulo 16777216.
foreign import data Mod16777216 :: Type

-- | Non-negative integers modulo 33554432.
foreign import data Mod33554432 :: Type

-- | Non-negative integers modulo 67108864.
foreign import data Mod67108864 :: Type

-- | Non-negative integers modulo 134217728.
foreign import data Mod134217728 :: Type

-- | Non-negative integers modulo 268435456.
foreign import data Mod268435456 :: Type

-- | Non-negative integers modulo 536870912.
foreign import data Mod536870912 :: Type

-- | Non-negative integers modulo 1073741824.
foreign import data Mod1073741824 :: Type

-- | Non-negative integers modulo 2147483648 - alternative to UInt.
foreign import data Mod2147483648 :: Type

-- | Non-negative integers modulo 4294967296 - alternative to UInt.
foreign import data Mod4294967296 :: Type


type Byte = Int / Mod256

type Nibble = Int / Mod65536

type Word = Number / Mod4294967296

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

instance canonicalMod2048 :: Canonical Int Mod2048 where
  canonical _ = abs <<< (_ `mod` 2048)

instance canonicalMod4096 :: Canonical Int Mod4096 where
  canonical _ = abs <<< (_ `mod` 4096)

instance canonicalMod8192 :: Canonical Int Mod8192 where
  canonical _ = abs <<< (_ `mod` 8192)

instance canonicalMod16384 :: Canonical Int Mod16384 where
  canonical _ = abs <<< (_ `mod` 16384)

instance canonicalMod32768 :: Canonical Int Mod32768 where
  canonical _ = abs <<< (_ `mod` 32768)

instance canonicalMod65536 :: Canonical Int Mod65536 where
  canonical _ = abs <<< (_ `mod` 65536)

instance canonicalMod131072 :: Canonical Int Mod131072 where
  canonical _ = abs <<< (_ `mod` 131072)

instance canonicalMod262144 :: Canonical Int Mod262144 where
  canonical _ = abs <<< (_ `mod` 262144)

instance canonicalMod524288 :: Canonical Int Mod524288 where
  canonical _ = abs <<< (_ `mod` 524288)

instance canonicalMod1048576 :: Canonical Int Mod1048576 where
  canonical _ = abs <<< (_ `mod` 1048576)

instance canonicalMod2097152 :: Canonical Int Mod2097152 where
  canonical _ = abs <<< (_ `mod` 2097152)

instance canonicalMod4194304 :: Canonical Int Mod4194304 where
  canonical _ = abs <<< (_ `mod` 4194304)

instance canonicalMod8388608 :: Canonical Int Mod8388608 where
  canonical _ = abs <<< (_ `mod` 8388608)

instance canonicalMod16777216 :: Canonical Int Mod16777216 where
  canonical _ = abs <<< (_ `mod` 16777216)

instance canonicalMod33554432 :: Canonical Int Mod33554432 where
  canonical _ = abs <<< (_ `mod` 33554432)

instance canonicalMod67108864 :: Canonical Int Mod67108864 where
  canonical _ = abs <<< (_ `mod` 67108864)

instance canonicalMod134217728 :: Canonical Int Mod134217728 where
  canonical _ = abs <<< (_ `mod` 134217728)

instance canonicalMod268435456 :: Canonical Int Mod268435456 where
  canonical _ = abs <<< (_ `mod` 268435456)

instance canonicalMod536870912 :: Canonical Int Mod536870912 where
  canonical _ = abs <<< (_ `mod` 536870912)

instance canonicalMod1073741824 :: Canonical Int Mod1073741824 where
  canonical _ = abs <<< (_ `mod` 1073741824)

instance canonicalMod2147483648 :: Canonical Number Mod2147483648 where
  canonical _ = abs <<< (_ `mod` 2147483648.0)

instance canonicalMod4294967296 :: Canonical Number Mod4294967296 where
  canonical _ = abs <<< (_ `mod` 4294967296.0)
