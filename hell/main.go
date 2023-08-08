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

// Ideally some enum/union/sum type, but I don't know how to do that in Go...
type serverResponse struct {
	Switch 		*switchResponse
	State 		*stateResponse
}

const port = 80

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
		log.Println("Error upgrading request: %v\n", err)
		return
	}
	log.Println("Client connected")
	defer conn.Close()

	// Add player
	var req request
	err = conn.ReadJSON(&req)
	if err != nil {
		if websocket.IsUnexpectedCloseError(err) {
			log.Printf("Client disconnected (before joining)")
		} else {
			log.Printf("Error reading from websocket: %v\n", err)
		}
		return
	}
	if req.Action != "join" {
		log.Printf("Must join as the first action: %s", req.Action)
		return
	}

	resChan := game.addPlayer(req.Name)
	log.Println("Player joined")

	go handleWrites(conn, resChan)

	for {
		var req request
		err := conn.ReadJSON(&req)
		if err != nil {
			if websocket.IsUnexpectedCloseError(err) {
				log.Printf("Client disconnected")
			} else {
				log.Printf("Error reading from websocket: %v\n", err)
			}
			return
		}
		game.in <- req
	}
}

func handleWrites(conn *websocket.Conn, resChan chan serverResponse) {
	// Handle all sends at the same time, to ensure we synchronize writes to
	// the connection.
	for res := range resChan {
		if res.Switch != nil {
			err := conn.WriteJSON(res.Switch)
			if err != nil {
				if !websocket.IsUnexpectedCloseError(err) {
					log.Printf("Error writing to websocket: %v\n", err)
				}
				break
			}
		}
		if res.State != nil {
			err := conn.WriteJSON(res.State)
			if err != nil {
				if !websocket.IsUnexpectedCloseError(err) {
					log.Printf("Error writing to websocket: %v\n", err)
				}
				break
			}
		}
	}

	for range resChan {
		// Ignore future packages after an error, until the channel closes
	}
}
