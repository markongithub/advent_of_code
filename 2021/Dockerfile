FROM haskell:9
# Running it before anything else installs GHC. Yes, the image also includes GHC. This is stupid.
RUN stack setup
COPY aoc2021/stack.yaml /code/stack.yaml
COPY aoc2021/package.yaml /code/package.yaml
COPY aoc2021/aoc2021.cabal /code/aoc2021.cabal
WORKDIR /code
RUN stack setup
RUN stack test --only-dependencies
COPY aoc2021 /code
RUN stack test
