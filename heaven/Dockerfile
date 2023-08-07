FROM haskell:9.4.5

workdir /app

COPY . ./

RUN stack install

CMD stack run
