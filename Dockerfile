FROM haskell:8.10.1-buster

WORKDIR /opt
COPY . .

RUN stack setup
