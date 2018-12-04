module Test.Main where

import Type.Quotient
  ( Quotient, class Canonical, runQuotient, mkQuotient, type (/)
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
import Test.QuickCheck (quickCheck)



main :: Effect Unit
main = do
  log "Running tests..."
  log "  - idempotent: ∀ x. run x == run (mk (run x))"
  log "    - Int / Id:"
  quickCheck (idempotent :: Int / Id -> Boolean)
  log "    - Int / Abs:"
  quickCheck (idempotent :: Int / Abs -> Boolean)
  log "    - Int / Mod2:"
  quickCheck (idempotent :: Int / Mod2 -> Boolean)
  log "    - Int / Mod4:"
  quickCheck (idempotent :: Int / Mod4 -> Boolean)
  log "    - Int / Mod8:"
  quickCheck (idempotent :: Int / Mod8 -> Boolean)
  log "    - Int / Mod16:"
  quickCheck (idempotent :: Int / Mod16 -> Boolean)
  log "    - Int / Mod32:"
  quickCheck (idempotent :: Int / Mod32 -> Boolean)
  log "    - Int / Mod64:"
  quickCheck (idempotent :: Int / Mod64 -> Boolean)
  log "    - Int / Mod128:"
  quickCheck (idempotent :: Int / Mod128 -> Boolean)
  log "    - Int / Mod256:"
  quickCheck (idempotent :: Int / Mod256 -> Boolean)
  log "    - Int / Mod512:"
  quickCheck (idempotent :: Int / Mod512 -> Boolean)
  log "    - Int / Mod1024:"
  quickCheck (idempotent :: Int / Mod1024 -> Boolean)
  log "    - Int / Mod2048:"
  quickCheck (idempotent :: Int / Mod2048 -> Boolean)
  log "    - Int / Mod4096:"
  quickCheck (idempotent :: Int / Mod4096 -> Boolean)
  log "    - Int / Mod8192:"
  quickCheck (idempotent :: Int / Mod8192 -> Boolean)
  log "    - Int / Mod16384:"
  quickCheck (idempotent :: Int / Mod16384 -> Boolean)
  log "    - Int / Mod32768:"
  quickCheck (idempotent :: Int / Mod32768 -> Boolean)
  log "    - Int / Mod65536:"
  quickCheck (idempotent :: Int / Mod65536 -> Boolean)
  log "    - Int / Mod131072:"
  quickCheck (idempotent :: Int / Mod131072 -> Boolean)
  log "    - Int / Mod262144:"
  quickCheck (idempotent :: Int / Mod262144 -> Boolean)
  log "    - Int / Mod524288:"
  quickCheck (idempotent :: Int / Mod524288 -> Boolean)
  log "    - Int / Mod1048576:"
  quickCheck (idempotent :: Int / Mod1048576 -> Boolean)
  log "    - Int / Mod2097152:"
  quickCheck (idempotent :: Int / Mod2097152 -> Boolean)
  log "    - Int / Mod4194304:"
  quickCheck (idempotent :: Int / Mod4194304 -> Boolean)
  log "    - Int / Mod8388608:"
  quickCheck (idempotent :: Int / Mod8388608 -> Boolean)
  log "    - Int / Mod16777216:"
  quickCheck (idempotent :: Int / Mod16777216 -> Boolean)
  log "    - Int / Mod33554432:"
  quickCheck (idempotent :: Int / Mod33554432 -> Boolean)
  log "    - Int / Mod67108864:"
  quickCheck (idempotent :: Int / Mod67108864 -> Boolean)
  log "    - Int / Mod134217728:"
  quickCheck (idempotent :: Int / Mod134217728 -> Boolean)
  log "    - Int / Mod268435456:"
  quickCheck (idempotent :: Int / Mod268435456 -> Boolean)
  log "    - Int / Mod536870912:"
  quickCheck (idempotent :: Int / Mod536870912 -> Boolean)
  log "    - Int / Mod1073741824:"
  quickCheck (idempotent :: Int / Mod1073741824 -> Boolean)
  log "    - Number / Mod2147483648:"
  quickCheck (idempotent :: Number / Mod2147483648 -> Boolean)
  log "    - Number / Mod4294967296:"
  quickCheck (idempotent :: Number / Mod4294967296 -> Boolean)


idempotent :: ∀ a e. Canonical a e => Eq a => Quotient a e -> Boolean
idempotent x =
  runQuotient x == runQuotient (mkQuotient (runQuotient x) :: a / e)
