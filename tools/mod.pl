#!/usr/bin/env perl
use strict;
use warnings;

for (1 .. 32) {
  printf "  , Mod%d\n", 2 ** $_;
}

for (1 .. 32) {
  printf "-- | Non-negative integers modulo %d.\n", 2 ** $_;
  printf "foreign import data Mod%d :: Type\n", 2 ** $_;
  printf "\n";
}

for (1 .. 32) {
  printf "instance canonicalMod%d :: Canonical Int Mod%d where\n", 2 ** $_, 2 ** $_;
  printf "  canonical _ = abs <<< (_ `mod` %d)\n", 2 ** $_;
  printf "\n";
}

for (1 .. 32) {
    printf "  log \"    - Int / Mod%d:\"\n", 2 ** $_;
    printf "  quickCheck (idempotent :: Int / Mod%d -> Boolean)\n", 2 ** $_;
}
