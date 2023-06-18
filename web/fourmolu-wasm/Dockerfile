FROM brandonchinn178/ghc-wasm-backend

RUN curl https://sh.rustup.rs -sSf | sh -s -- -y
ENV PATH=/root/.cargo/bin:$PATH
RUN cargo install wizer --version=1.6.0 --all-features

RUN ghcup install cabal 3.10
