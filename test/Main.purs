module Test.Main where

import Type.Quotient
  ( Quotient, class Canonical, canonical, runQuotient, mkQuotient, type (/)
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
  )

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Test.QuickCheck (quickCheck, (===), Result)
import Type.Proxy (Proxy (..))



main :: Effect Unit
main = do
  log "Running tests..."
  idempotentTests



idempotentTests :: Effect Unit
idempotentTests = do
  log "  - idempotent: ∀ (x :: a where Canonical a e). canonical x == canonical (canonical x)"
  log "    - Int / Mod2:"
  quickCheck (idempotent (Proxy :: Proxy Mod2))
  log "    - Int / Mod4:"
  quickCheck (idempotent (Proxy :: Proxy Mod4))
  log "    - Int / Mod8:"
  quickCheck (idempotent (Proxy :: Proxy Mod8))
  log "    - Int / Mod16:"
  quickCheck (idempotent (Proxy :: Proxy Mod16))
  log "    - Int / Mod32:"
  quickCheck (idempotent (Proxy :: Proxy Mod32))
  log "    - Int / Mod64:"
  quickCheck (idempotent (Proxy :: Proxy Mod64))
  log "    - Int / Mod128:"
  quickCheck (idempotent (Proxy :: Proxy Mod128))
  log "    - Int / Mod256:"
  quickCheck (idempotent (Proxy :: Proxy Mod256))
  log "    - Int / Mod512:"
  quickCheck (idempotent (Proxy :: Proxy Mod512))
  log "    - Int / Mod1024:"
  quickCheck (idempotent (Proxy :: Proxy Mod1024))
  log "    - Int / Mod2048:"
  quickCheck (idempotent (Proxy :: Proxy Mod2048))
  log "    - Int / Mod4096:"
  quickCheck (idempotent (Proxy :: Proxy Mod4096))
  log "    - Int / Mod8192:"
  quickCheck (idempotent (Proxy :: Proxy Mod8192))
  log "    - Int / Mod16384:"
  quickCheck (idempotent (Proxy :: Proxy Mod16384))
  log "    - Int / Mod32768:"
  quickCheck (idempotent (Proxy :: Proxy Mod32768))
  log "    - Int / Mod65536:"
  quickCheck (idempotent (Proxy :: Proxy Mod65536))
  log "    - Int / Mod131072:"
  quickCheck (idempotent (Proxy :: Proxy Mod131072))
  log "    - Int / Mod262144:"
  quickCheck (idempotent (Proxy :: Proxy Mod262144))
  log "    - Int / Mod524288:"
  quickCheck (idempotent (Proxy :: Proxy Mod524288))
  log "    - Int / Mod1048576:"
  quickCheck (idempotent (Proxy :: Proxy Mod1048576))
  log "    - Int / Mod2097152:"
  quickCheck (idempotent (Proxy :: Proxy Mod2097152))
  log "    - Int / Mod4194304:"
  quickCheck (idempotent (Proxy :: Proxy Mod4194304))
  log "    - Int / Mod8388608:"
  quickCheck (idempotent (Proxy :: Proxy Mod8388608))
  log "    - Int / Mod16777216:"
  quickCheck (idempotent (Proxy :: Proxy Mod16777216))
  log "    - Int / Mod33554432:"
  quickCheck (idempotent (Proxy :: Proxy Mod33554432))
  log "    - Int / Mod67108864:"
  quickCheck (idempotent (Proxy :: Proxy Mod67108864))
  log "    - Int / Mod134217728:"
  quickCheck (idempotent (Proxy :: Proxy Mod134217728))
  log "    - Int / Mod268435456:"
  quickCheck (idempotent (Proxy :: Proxy Mod268435456))
  log "    - Int / Mod536870912:"
  quickCheck (idempotent (Proxy :: Proxy Mod536870912))
  log "    - Int / Mod1073741824:"
  quickCheck (idempotent (Proxy :: Proxy Mod1073741824))
  log "    - Number / Mod2147483648:"
  quickCheck (idempotent (Proxy :: Proxy Mod2147483648))
  log "    - Number / Mod4294967296:"
  quickCheck (idempotent (Proxy :: Proxy Mod4294967296))

idempotent :: ∀ a e
            . Show a
           => Canonical a e
           => Eq a
           => Proxy e -> a -> Result
idempotent p x =
  let can = canonical p
  in  can x === can (can x)
