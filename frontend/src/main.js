const width = 20
const height = 20

// Enabled when the hostname is not set, i.e. likely a file system path
const isDev = !location.host

// First one here will be the fallback theme
const knownThemes = ['snake', 'hell', 'sketch', 'heaven']
const protocol =  location.protocol.startsWith('https') ? 'wss' : 'ws'
const servers = [
  `${protocol}://${location.host}/hell`,
  `${protocol}://${location.host}/snake`,
  `${protocol}://${location.host}/sketch`,
  // `${protocol}://${location.host}/heaven`,
  `${protocol}://${location.host}/star`,
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
  const projectileCanvas = document.getElementById('projectile-canvas')
  const legend = document.getElementById('legend-data')

  let currentPlayerData = []

  const createPlayerDiv = () => {
    const elem = document.createElement('div')
    elem.className = `player`
    const img = document.createElement('img')
    elem.appendChild(img)
    return elem
  }

  const populatePlayerDiv = (elem, theme, name) => {
    const pct = nameToDeg(name)

    let imgUrl = `../sprites/${theme}/player.png`
    if (isDev) {
      // Workaround for the weirdest bug that I have seen:
      // File-system urls don't work in `mask`!
      imgUrl = `https://raw.githubusercontent.com/kofoednielsen/pass/main/frontend/sprites/${theme}/player.png`
    }

    elem.style.webkitMaskImage = `url("${imgUrl}")`
    elem.style.maskImage = `url("${imgUrl}")`
    elem.style.backgroundColor = `hsl(${pct}deg 100% 40%)`
    elem.firstChild.src = imgUrl
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

    // Add missing players
    while (state.players.length > playerCanvas.childElementCount) {
      playerCanvas.appendChild(createPlayerDiv())
    }
    // Delete excess players
    while (state.players.length < playerCanvas.childElementCount) {
      playerCanvas.removeChild(playerCanvas.lastChild)
    }
    // Update properties
    for (let i = 0; i < state.players.length; i++) {
      const player = state.players[i]
      const elem = playerCanvas.children[i]
      populatePlayerDiv(elem, theme, player.name)
      elem.style.marginLeft = `${player.position.x * 100 / width}%`
      elem.style.marginTop = `${player.position.y * 100 / height}%`
    }

    // Add missing projectiles
    while (state.projectiles.length > projectileCanvas.childElementCount) {
      const elem = document.createElement('div')
      elem.className = "projectile"
      const img = document.createElement('img')
      elem.appendChild(img)
      projectileCanvas.appendChild(elem)
    }
    // Delete excess projectiles
    while (state.projectiles.length < projectileCanvas.childElementCount) {
      projectileCanvas.removeChild(projectileCanvas.lastChild)
    }
    // Update properties
    for (let i = 0; i < state.projectiles.length; i++) {
      const proj = state.projectiles[i]
      const elem = projectileCanvas.children[i]
      elem.firstChild.src = `../sprites/${theme}/projectile.png`
      elem.style.marginLeft = `${proj.x * 100 / width}%`
      elem.style.marginTop = `${proj.y * 100 / height}%`
    }

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
        const elem = createPlayerDiv()
        populatePlayerDiv(elem, theme, name)
        legendChildren.push(elem)

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
