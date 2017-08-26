FROM haskell:8

RUN mkdir -p /app/user
WORKDIR /app/user

COPY stack.yaml .
COPY *.cabal ./
RUN stack build --dependencies-only

COPY . /app/user
RUN stack install

