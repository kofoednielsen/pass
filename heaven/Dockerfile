FROM haskell:9.4.5

workdir /app

COPY . ./

RUN stack build

CMD stack run
