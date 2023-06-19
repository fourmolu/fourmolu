# fourmolu.github.io

This directory contains files that are served at `fourmolu.github.io`.

## Quickstart

1. Build `fourmolu-wasm` (see below)
1. `web/worker/build.sh`
1. `(cd web/site && cabal run -- fourmolu-web watch)`
1. Open `http://localhost:8000` in a browser

## Build `fourmolu-wasm`

### Build locally

1. Follow the steps at https://gitlab.haskell.org/ghc/ghc-wasm-meta to install GHC with the WASM backend

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

### Troubleshooting

* `SIMD support is not enabled`
    * This is a known bug on aarch64: https://gitlab.haskell.org/ghc/ghc/-/issues/23410
