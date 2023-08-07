package main

import (
	"fmt"
	"log"
	"net/http"

	"github.com/gorilla/websocket"
)

type request struct {
	Action string `json:"action"`
	Name   string `json:"name"`
}

type switchResponse struct {
	Event string `json:"event"`
	Name  string `json:"name"`
}

type stateResponse struct {
	Event       string     `json:"event"`
	Theme       string     `json:"theme"`
	Name        string     `json:"name"`
	Players     []player   `json:"players"`
	Projectiles []position `json:"projectiles"`
}

const port = 8080

var game *hell

func main() {
	fmt.Println("Generating game world...")
	game = newHell()
	go game.run()

	// http.HandleFunc("/", rootHandler)
	http.HandleFunc("/", hellHandler)

	fmt.Println("Listening on port", port)
	log.Fatal(http.ListenAndServe(fmt.Sprintf(":%d", port), nil))
}

// func rootHandler(w http.ResponseWriter, r *http.Request) {
// 	fmt.Fprintf(w, "Welcome to Hell...")
// }

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

func hellHandler(w http.ResponseWriter, r *http.Request) {
	upgrader.CheckOrigin = func(r *http.Request) bool { return true }
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Println(err)
		return
	}
	defer conn.Close()
	log.Println("Client Connected")

	// Add player
	var req request
	err = conn.ReadJSON(&req)
	if err != nil || req.Action != "join" {
		log.Printf("Error reading from websocket: %s", err)
		return
	}
	quit := make(chan struct{})
	resChan := game.addPlayer(req.Name, quit)
	log.Println("Player added")

	go handleReads(conn)
	go handleWrites(conn, resChan)
	<-quit
	res := switchResponse{
		Event: "switch",
		Name:  req.Name,
	}
	err = conn.WriteJSON(&res)
	if err != nil {
		log.Println(err)
		return
	}

	return
}

func handleReads(conn *websocket.Conn) {
	for {
		var req request
		err := conn.ReadJSON(&req)
		if err != nil {
			log.Printf("Client sent unexpected message: %v\n", err)
		}
		game.in <- req
	}
}

func handleWrites(conn *websocket.Conn, resChan chan stateResponse) {
	for res := range resChan {
		err := conn.WriteJSON(&res)
		if err != nil {
			log.Printf("Client didn't receive message: %v\n", err)
		}
	}
}
