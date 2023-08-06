package main

import (
	"time"
)

const (
	mapHeight = 20
	mapWidth  = 20
)

//var game game

type position struct {
	X int `json:"x"`
	Y int `json:"y"`
}

type player struct {
	Name       string   `json:"name"`
	Invincible bool     `json:"invincible"`
	Pos        position `json:"position"`
	Health     float64  `json:"health"`
	direction  float64
	quit       chan struct{}
	out        chan stateResponse
}

type hell struct {
	in          chan request
	clock       *time.Ticker
	players     map[string]player
	projectiles []position
}

func newHell() *hell {
	return &hell{
		clock: time.NewTicker(100 * time.Millisecond),
		in:    make(chan request, 10),
	}
}

func (h *hell) addPlayer(name string, quit chan struct{}) chan stateResponse {
	player := player{
		Name:   name,
		Health: 1.0,
		quit:   quit,
		out:    make(chan stateResponse),
	}
	// TODO: Initialize position and direction
	h.players[name] = player
	return player.out
}

func (h *hell) run() {

	for {
		h.updateFromInput()

		// TODO: Update game state based on mechanics/time

		for k, p := range h.players {
			if p.Health == 0 {
				h.kill(k)
			}
		}

		<-h.clock.C

		res := stateResponse{
			// TODO: Construct response struct
		}

		for _, p := range h.players {
			p.out <- res
		}
	}

}

func (h *hell) updateFromInput() {
	for {
		select {
		case req := <-h.in:
			switch req.Action {
			case "up":
				// TODO: Handle up
			case "down":
				// TODO: Handle down
			case "left":
				// TODO: Handle left
			case "right":
				// TODO: Handle right
			case "attack":
				// TODO: Handle attack
			case "join":
				// TODO: Handle join
			default:
				// bad client
			}
		default:
			// Queue is empty
			return
		}
	}
}

func (h *hell) kill(player string) {
	h.players[player].quit <- struct{}{}
	delete(h.players, player)
}
