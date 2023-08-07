const width = 20
const height = 20

type ClientRequest = {
  name: string
  action: "up" | "down" | "left" | "right" | "attack" | "join"
}

type ServerSwitch = {
  event: "switch"
  name: string
}

/** Coordinate space: (0, 0) is top right corner */
type Position = {
  x: number
  y: number
}

type Player = {
  name: string
  suffx: string
  invincible: boolean
  position: Position
  health: number
}

type ServerState = {
  event: "state"
  theme: string
  players: Player[]
  projectiles: Position[]
}

type ServerResponse = ServerSwitch | ServerState

// ---

function addr_to_string(addr: Deno.Addr): string {
  const net_addr = addr as Deno.NetAddr
  return `${net_addr.hostname}:${net_addr.port}`
  // todo: Support Deno.UnixAddr
  // return addr.path
}

// ---

// map[x][y] === painted or non-painted tile
const map: boolean[][] = []
for (let x = 0; x < width; x++) {
  map[x] = []
  for (let y = 0; y < height; y++) {
    map[x][y] = false
  }
}

type Client = {
  addr: Deno.Addr
  sender: (response: ServerResponse) => void
  players: {
    [name: string]: {
      position: Position
    }
  }
}

const clients: Client[] = []

function getCurrentState(): ServerState {
  const players = []
  for (const client of clients) {
    for (const [name, player] of Object.entries(client.players)) {
      players.push({
        name,
        suffx: "the artist",
        invincible: false,
        position: player.position,
        health: 1.0,
      })
    }
  }

  const projectiles: Position[] = []
  for (let x = 0; x < width; x++) {
    for (let y = 0; y < height; y++) {
      if (map[x][y]) {
        projectiles.push({ x, y })
      }
    }
  }

  return {
    event: "state",
    theme: "sketch",
    players,
    projectiles,
  }
}

function handle(socket: WebSocket, addr: Deno.Addr) {
  const send = (response: ServerResponse) => {
    socket.send(JSON.stringify(response))
  }

  let client: Client | undefined = undefined

  socket.onopen = () => {
    console.info(`Connect: ${addr_to_string(addr)}`)
    client = { addr, sender: send, players: {} }
    clients.push(client!)
    send(getCurrentState())
  }

  socket.onmessage = (e) => {
    console.info(`Request: ${addr_to_string(addr)}, ${e.data}`)
    const request = JSON.parse(e.data) as ClientRequest

    if (request.action === "join") {
      const x = Math.round(width / 2)
      const y = Math.round(height / 2)
      client!.players[request.name] = { position: { x, y } }
    } else {
      const player = client!.players[request.name]

      if (player === undefined) {
        console.error(`${request.name} tried to do '${request.action}' before joining`)
        return
      }

      switch (request.action) {
        case "up": {
          player.position.y -= 1
          break
        }
        case "down": {
          player.position.y += 1
          break
        }
        case "left": {
          player.position.x -= 1
          break
        }
        case "right": {
          player.position.x += 1
          break
        }
        case "attack": {
          const position = player.position
          // Paint or unpaint the current tile
          map[position.x][position.y] = !map[position.x][position.y]
          break
        }
      }
    }

    // Switch server when going out of bounds
    const position = client!.players[request.name].position
    if (
      position.x < 0 || position.x > width || position.y < 0 ||
      position.y > height
    ) {
      delete client!.players[request.name]
      send({
        event: "switch",
        name: request.name,
      })
    }

    // Publish state change to all clients
    const serverState = getCurrentState()
    for (const client of clients) {
      client.sender(serverState)
    }
  }

  socket.onclose = () => {
    console.info(`Disconnect: ${addr_to_string(addr)}`)
    const index = clients.indexOf(client!)
    if (index === -1) {
      throw "can only disconnect once, and only after connecting"
    }
    clients.splice(index, 1)
  }

  socket.onerror = (e) => console.error(`WebSocket error: ${e}`)
}

const listener = Deno.listen({ port: 8080 })

console.info(`Listening on: ${addr_to_string(listener.addr)}`)

for await (const conn of listener) {
  console.info(
    `Incoming TCP connection from: ${addr_to_string(conn.remoteAddr)}`,
  )

  const httpConn = Deno.serveHttp(conn)
  for await (const requestEvent of httpConn) {
    const upgrade = requestEvent.request.headers.get("upgrade") || ""
    let response
    if (upgrade.toLowerCase() != "websocket") {
      response = new Response("request isn't trying to upgrade to websocket.")
    } else {
      const upgrade = Deno.upgradeWebSocket(requestEvent.request)
      handle(upgrade.socket, conn.remoteAddr)
      response = upgrade.response
    }
    await requestEvent.respondWith(response)
  }
}
