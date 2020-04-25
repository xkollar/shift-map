#!/usr/bin/env bash

what=${1:-test}

if [[ "${what}" == 'test' ]]; then
    ghcid \
        --restart=shift-map.cabal \
        --restart=stack.yaml \
        --restart=package.yaml \
        --test=main \
        --command="stack --no-nix --docker ghci shift-map:test:unit shift-map:lib"
elif [[ "${what}" == "bench" ]]; then
    ghcid \
        --restart=shift-map.cabal \
        --restart=stack.yaml \
        --restart=package.yaml \
        --test=main \
        --command="stack --no-nix --docker ghci shift-map:bench:benchmarks shift-map:lib"
else
    echo "Unknown target: ${what}"
    exit 1
fi
