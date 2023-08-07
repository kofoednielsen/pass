from golang

workdir /app

COPY go.mod go.sum ./
RUN go mod download

COPY *.go ./

RUN go build

CMD ./hell
