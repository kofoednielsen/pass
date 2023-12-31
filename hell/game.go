package main

import (
	"log"
	"time"
)

const (
	mapHeight          = 20
	mapWidth           = 20
	projectileLifetime = 20
)

type position struct {
	X int `json:"x"`
	Y int `json:"y"`
}

func (p position) Up() position {
	return position{p.X, (p.Y - 1)}
}

func (p position) Down() position {
	return position{p.X, (p.Y + 1)}
}

func (p position) Left() position {
	return position{(p.X - 1), p.Y}
}

func (p position) Right() position {
	return position{(p.X + 1), p.Y}
}

type player struct {
	Name       string   `json:"name"`
	Invincible bool     `json:"invincible"`
	Pos        position `json:"position"`
	Health     int64    `json:"health"`
	out        chan serverResponse
	birthday   time.Time
}

type projectile struct {
	birthday time.Time
	X        int `json:"x"`
	Y        int `json:"y"`
}

func (p *player) update() {
	p.Invincible = time.Since(p.birthday) < 5*time.Second
}

type hell struct {
	in               chan request
	clock            *time.Ticker
	globalDamageTick int
	players          map[string]player
	projectiles      []projectile
	listeners        []chan serverResponse
}

func newHell() *hell {
	return &hell{
		clock:            time.NewTicker(100 * time.Millisecond),
		globalDamageTick: 0,
		in:               make(chan request, 10),
		players:          make(map[string]player),
	}
}

func (h *hell) addListener(channel chan serverResponse) {
	h.listeners = append(h.listeners, channel)
}

func indexOf(listeners []chan serverResponse, channel chan serverResponse) int {
	for i, listener := range listeners {
		if listener == channel {
			return i
		}
	}
	return -1
}

func (h *hell) removeListener(channel chan serverResponse) {
	i := indexOf(h.listeners, channel)
	h.listeners = append(h.listeners[:i], h.listeners[i+1:]...)
}

func (h *hell) addPlayer(name string, channel chan serverResponse) {
	player := player{
		Health:     100,
		Invincible: true,
		Name:       name,
		Pos:        position{10, 10},
		birthday:   time.Now(),
		out:        channel,
	}
	h.players[name] = player
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

		surivingProjectiles := []projectile{}

		for _, proj := range h.projectiles {
			if time.Since(proj.birthday) < 200*time.Millisecond {
				surivingProjectiles = append(surivingProjectiles, proj)
			}
		}

		h.projectiles = surivingProjectiles

		res := stateResponse{
			Event:       "state",
			Players:     playerList,
			Projectiles: surivingProjectiles,
			Theme:       "hell",
		}

		for _, listener := range h.listeners {
			listener <- serverResponse{
				Switch: nil,
				State:  &res,
			}
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
				delete(h.players, p.Name)
			}

			if p.Pos.X >= mapWidth || p.Pos.X < 0 || p.Pos.Y >= mapHeight || p.Pos.Y < 0 {
				p.Health = 0
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
	for name, p := range h.players {
		switch p.Pos {
		case n, s, w, e:
			p.Health -= 15
		case nw, ne, sw, se:
			p.Health -= 10
		}

		for _, pos := range []position{n, s, w, e, nw, ne, sw, se} {
			h.projectiles = append(h.projectiles, projectile{X: pos.X, Y: pos.Y, birthday: time.Now()})
		}

		h.players[name] = p
	}
}

func (h *hell) kill(p string) {
	res := switchResponse{
		Event: "switch",
		Name:  h.players[p].Name,
	}
	h.players[p].out <- serverResponse{
		Switch: &res,
		State:  nil,
	}
	delete(h.players, p)
}
