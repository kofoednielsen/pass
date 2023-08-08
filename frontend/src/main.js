const width = 20
const height = 20

// Enabled when the hostname is not set, i.e. likely a file system path
const isDev = !location.host

// First one here will be the fallback theme
const knownThemes = ['snake', 'hell', 'sketch', 'heaven']
const servers = [
  `ws://${location.host}/hell`,
  `ws://${location.host}/snake`,
  `ws://${location.host}/sketch`,
  // `ws://${location.host}/heaven`,
]

var joined = false
var username = ""
var socket = null

const nameToDeg = (name) => {
    let hash = 0, chr
    for (let i = 0; i < name.length; i++) {
      chr = name.charCodeAt(i)
      hash = ((hash << 5) - hash) + chr
      hash |= 0
    }
    return hash % 360
}

const selectNextUrl = () => {
  if (isDev) {
    return `ws://localhost:8080/ws`
  }

  // Select one randomly, except the last one
  if (socket) {
    const tmpServers = servers.filter((elem) => elem !== socket.url)
    return tmpServers[Math.floor(Math.random() * tmpServers.length)]
  } else {
    return servers[Math.floor(Math.random() * servers.length)]
  }
}

const sendJoin = () => {
  const request = JSON.stringify({ name: username, action: 'join' })
  console.log(`sending: ${request}`)
  socket.send(request)
}

const beginGameAtUrl = (url) => {
  const joinButton = document.getElementById('join')
  const playerCanvas = document.getElementById('player-canvas')
  playerCanvas.style.gridTemplateColumns = `repeat(${width}, 20px)`
  playerCanvas.style.gridTemplateRows = `repeat(${height}, 20px)`
  const projectileCanvas = document.getElementById('projectile-canvas')
  projectileCanvas.style.gridTemplateColumns = `repeat(${width}, 20px)`
  projectileCanvas.style.gridTemplateRows = `repeat(${height}, 20px)`
  const legend = document.getElementById('legend-data')

  let currentPlayerData = []

  const createPlayerDiv = (theme, name) => {
    const pct = nameToDeg(name)

    let imgUrl = `../sprites/${theme}/player.png`
    if (isDev) {
      // Workaround for the weirdest bug that I have seen:
      // File-system urls don't work in `mask`!
      imgUrl = `https://raw.githubusercontent.com/kofoednielsen/pass/main/frontend/sprites/${theme}/player.png`
    }

    const elem = document.createElement('div')
    elem.className = `player`
    elem.style.webkitMaskImage = `url("${imgUrl}")`
    elem.style.maskImage = `url("${imgUrl}")`
    elem.style.backgroundColor = `hsl(${pct}deg 100% 40%)`
    const img = document.createElement('img')
    elem.appendChild(img)
    img.src = imgUrl

    return elem
  }

  socket = new WebSocket(url)

  socket.addEventListener("open", (event) => {
    console.info(`connected to ${socket.url}`)
    if (joined) {
      sendJoin()
    } else {
      joinButton.disabled = false
    }
  })

  socket.addEventListener('message', (event) => {
    console.debug(`received: ${event.data}`)
    const state = JSON.parse(event.data)
    if (state.event === 'switch') {
      socket.close()
      return
    }
    let theme
    if (knownThemes.indexOf(state.theme) === -1) {
      theme = knownThemes[0]
    } else {
      theme = state.theme
    }

    const playerCanvasChildren = []
    for (const player of state.players) {
      const elem = createPlayerDiv(theme, player.name)
      elem.style.gridColumnStart = player.position.x + 1
      elem.style.gridRowStart = player.position.y + 1
      playerCanvasChildren.push(elem)
    }
    playerCanvas.replaceChildren(...playerCanvasChildren)

    const projectileCanvasChildren = []
    for (const proj of state.projectiles) {
      const elem = document.createElement('img')
      projectileCanvasChildren.push(elem)
      elem.src = `../sprites/${theme}/projectile.png`
      elem.style.gridColumnStart = proj.x + 1
      elem.style.gridRowStart = proj.y + 1
    }
    projectileCanvas.replaceChildren(...projectileCanvasChildren)

    let deduplicatedPlayerData = []
    for (const player of state.players) {
      // Ugly ugly
      deduplicatedPlayerData.push(JSON.stringify([player.health, player.name]))
    }
    deduplicatedPlayerData = [...new Set(deduplicatedPlayerData)]
    for (const i in deduplicatedPlayerData) {
      deduplicatedPlayerData[i] = JSON.parse(deduplicatedPlayerData[i])
    }
    deduplicatedPlayerData.sort(([health1, name1], [health2, name2]) => {
      if (health1 > health2) {
        return false
      } else if (health1 < health2) {
        return true
      } else {
        return name1 > name2
      }
    })

    if (JSON.stringify(deduplicatedPlayerData) !== currentPlayerData) {
      currentPlayerData = JSON.stringify(deduplicatedPlayerData)

      const legendChildren = []
      for (const [health, name] of deduplicatedPlayerData) {
        legendChildren.push(createPlayerDiv(theme, name))

        const healthBar = document.createElement('div')
        healthBar.className = "healthBar"
        const healthBarInner = document.createElement('div')
        healthBar.appendChild(healthBarInner)
        healthBarInner.style.width = `${health}%`
        legendChildren.push(healthBar)

        const p = document.createElement('span')
        // Intentional vulnerability
        p.innerHTML = name
        legendChildren.push(p)
      }

      legend.replaceChildren(...legendChildren)
    }
  })

  socket.addEventListener('error', (event) => {
    console.error(`WebSocket error: ${event}`)
    socket.close()
  })

  socket.addEventListener("close", () => {
    console.info(`disconnected`)
    beginGameAtUrl(selectNextUrl())
  })
}

window.addEventListener("load", () => {
  const joinButton = document.getElementById('join')
  const usernameField = document.getElementById('username')
  usernameField.focus()

  beginGameAtUrl(selectNextUrl())

  usernameField.addEventListener("keypress", function(event) {
    // If the user presses the "Enter" key on the keyboard
    if (event.key === "Enter") {
      event.preventDefault()
      joinButton.click()
    }
  })

  joinButton.addEventListener('click', () => {
    const value = usernameField.value
    if (value) {
      username = value
      const username_input = document.querySelector(".username-input")
      username_input.style.visibility = 'hidden'
      joined = true
      sendJoin()
    }
  })

  document.addEventListener('keydown', (event) => {
      if (event.key && joined) {
        let action = ""
        if (event.key === ' ') {
          action = 'attack'
        } else if (event.key == 'ArrowUp' || event.key == 'w') {
          action = 'up'
        } else if (event.key == 'ArrowDown' || event.key == 's') {
          action = 'down'
        } else if (event.key == 'ArrowLeft' || event.key == 'a') {
          action = 'left'
        } else if (event.key == 'ArrowRight' || event.key == 'd') {
          action = 'right'
        }
        if (action) {
          const request = JSON.stringify({name: username, action})
          console.log(`sending: ${request}`)
          socket.send(request)
        }
      }
  })
})
