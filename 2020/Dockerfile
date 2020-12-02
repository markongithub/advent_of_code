FROM haskell:8
RUN stack setup
COPY aoc2020/aoc2020.cabal /code/aoc2020.cabal
COPY aoc2020/stack.yaml /code/stack.yaml
WORKDIR /code
# RUN stack build --only-dependencies
RUN stack test --only-dependencies
COPY aoc2020 /code
RUN stack test
