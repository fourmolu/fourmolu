FROM brandonchinn178/ghc-wasm-backend

RUN ghcup install cabal 3.10

RUN WIZER_VERSION=3.0.1 \
    && mkdir -p /opt/wizer \
    && cd /opt/wizer \
    && curl -fsSLO \
        https://github.com/bytecodealliance/wizer/releases/download/v${WIZER_VERSION}/wizer-v${WIZER_VERSION}-aarch64-linux.tar.xz \
    && tar xJf wizer-*.tar.xz --strip-components=1
ENV PATH=/opt/wizer:$PATH
