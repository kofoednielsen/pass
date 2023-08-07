package main

import (
	"log"
	"time"
)

const (
	mapHeight = 20
	mapWidth  = 20
)

type position struct {
	X int `json:"x"`
	Y int `json:"y"`
}

func (p position) Up() position {
	return position{p.X, (p.Y + mapHeight - 1) % mapHeight}
}

func (p position) Down() position {
	return position{p.X, (p.Y + 1) % mapHeight}
}

func (p position) Left() position {
	return position{(p.X + mapWidth - 1) % mapWidth, p.Y}
}

func (p position) Right() position {
	return position{(p.X + 1) % mapWidth, p.Y}
}

type player struct {
	Name       string   `json:"name"`
	Invincible bool     `json:"invincible"`
	Pos        position `json:"position"`
	Health     float64  `json:"health"`
	quit       chan bool
	out        chan stateResponse
	birthday   time.Time
}

func (p *player) update() {
	p.Invincible = time.Since(p.birthday) < 5*time.Second
}

type hell struct {
	in          chan request
	clock       *time.Ticker
	players     map[string]player
	projectiles []position
}

func newHell() *hell {
	return &hell{
		clock:   time.NewTicker(100 * time.Millisecond),
		in:      make(chan request, 10),
		players: make(map[string]player),
	}
}

func (h *hell) addPlayer(name string, quit chan bool) chan stateResponse {
	player := player{
		Health:     1.0,
		Invincible: true,
		Name:       name,
		Pos:        position{10, 10},
		birthday:   time.Now(),
		out:        make(chan stateResponse),
		quit:       quit,
	}
	h.players[name] = player
	return player.out
}

func (h *hell) run() {

	for {
		h.updateFromInput()

		for name, p := range h.players {
			if p.Health <= 0 {
				h.kill(name)
			}
		}

		<-h.clock.C

		playerList := make([]player, 0)
		for name, p := range h.players {
			p.update()
			playerList = append(playerList, p)
			h.players[name] = p
		}
		res := stateResponse{
			Event:       "state",
			Players:     playerList,
			Projectiles: []position{},
			Theme:       "hell",
		}

		for _, p := range playerList {
			p.out <- res
		}
	}
}

func (h *hell) updateFromInput() {
	for {
		select {
		case req := <-h.in:
			p, ok := h.players[req.Name]
			if !ok {
				log.Printf("Unknown player name \"%s\"\n", req.Name)
			}
			switch req.Action {
			case "up":
				p.Pos = p.Pos.Up()
			case "down":
				p.Pos = p.Pos.Down()
			case "left":
				p.Pos = p.Pos.Left()
			case "right":
				p.Pos = p.Pos.Right()
			case "attack":
				h.dealDamage(p.Pos)
			case "join":
				log.Printf("Player \"%s\" sent join after already joining\n", p.Name)
			default:
				log.Printf("Player \"%s\" sent unknown action \"%s\"\n", p.Name, req.Action)
				p.quit <- false
				delete(h.players, p.Name)
			}
			h.players[req.Name] = p
		default:
			// Queue is empty
			return
		}
	}
}

func (h *hell) dealDamage(pos position) {
	n, s, w, e := pos.Up(), pos.Down(), pos.Left(), pos.Right()
	nw, ne, sw, se := n.Left(), n.Right(), s.Left(), s.Right()
	for _, p := range h.players {
		switch p.Pos {
		case n, s, w, e:
			p.Health -= 0.15
		case nw, ne, sw, se:
			p.Health -= 0.10
		}
	}
}

func (h *hell) kill(p string) {
	h.players[p].quit <- true
	delete(h.players, p)
}
