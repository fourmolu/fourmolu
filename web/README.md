# fourmolu.github.io

This directory contains files that are served at `fourmolu.github.io`.

## Quickstart

1. Build `fourmolu-wasm` (see below)
1. `web/worker/build.sh`
1. `(cd web/site && cabal run -- fourmolu-web watch)`
1. Open `http://localhost:8000` in a browser

## Build `fourmolu-wasm`

### Get from CI

1. Push a commit to GitHub
1. Navigate to the "Fourmolu website" workflow
1. Download the `fourmolu-wasm` artifact
1. Unzip it and put `fourmou-wasm.wasm` in `web/site/static/`

### Build locally

1. Follow the steps at https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta to install GHC with the WASM backend

1. Install [Wizer](https://github.com/bytecodealliance/wizer)

1. `wasm32-wasi-cabal update`

1. `web/fourmolu-wasm/build.sh`

### Build with Docker

If it's too difficult to install locally, you can run the WASM build in a Docker container:

```bash
# in the root directory of the fourmolu repo
docker build -t fourmolu-wasm -f web/fourmolu-wasm/Dockerfile .
docker run --rm -it \
    -v $PWD:/src \
    -v /tmp/fourmolu-wasm-cabal-cache:/root/.wasm32-wasi-cabal \
    -e CABAL_DIR=/root/.wasm32-wasi-cabal \
    --entrypoint bash \
    fourmolu-wasm
```

```bash
# in the Docker container
wasm32-wasi-cabal update
/src/web/fourmolu-wasm/build.sh
```
