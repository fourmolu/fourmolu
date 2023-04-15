# fourmolu.github.io

This directory contains files that are served at `fourmolu.github.io`.

## Quickstart

1. Build `fourmolu-wasm` (see below)
1. `web/worker/build.sh`
1. `(cd web/site && cabal run -- fourmolu-web watch)`
1. Open `http://localhost:8000` in a browser

## Build `fourmolu-wasm`

### Build locally

To build locally, follow the steps at https://gitlab.haskell.org/ghc/ghc-wasm-meta to install GHC with the WASM backend. Then run the following steps:

```bash
wasm32-wasi-cabal update
web/fourmolu-wasm/build.sh
```

### Build with Docker

If it's too difficult to install GHC with WASM locally, you can run the WASM build in a Docker container:

```bash
# host machine
docker run --rm -it \
    -v $PWD:/src \
    -v /tmp/fourmolu-wasm-cabal-cache:/root/.wasm32-wasi-cabal \
    -e CABAL_DIR=/root/.wasm32-wasi-cabal \
    --entrypoint bash \
    brandonchinn178/ghc-wasm-backend

# in docker
wasm32-wasi-cabal update
/src/web/fourmolu-wasm/build.sh
```
