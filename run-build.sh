#!/usr/bin/env bash

stack \
    --no-nix \
    --docker \
    build \
    --pedantic \
    --force-dirty \
    --test \
    --bench \
    --benchmark-arguments="--output bench-results.html" \
    --ghc-options "-ddump-to-file -ddump-hi" \
    --haddock

