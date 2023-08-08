from golang

RUN go install github.com/codegangsta/gin@2c98d96c9244c7426e985119b522f6e85c4bc81f
workdir /app

COPY go.mod go.sum ./
RUN go mod download

COPY *.go ./

RUN go build

CMD gin --port 80 run hell/main.go
