FROM haskell:9
COPY aoc2021 /code
WORKDIR /code
RUN stack setup
RUN stack build --test
