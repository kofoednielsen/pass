const width = 20
const height = 20

// Enabled when the hostname is not set, i.e. likely a file system path
const isDev = !location.host

// First one here will be the fallback theme
const knownThemes = ['snake']
const servers = [
  `ws://${location.host}/snake`,
  `ws://${location.host}/sketch`,
]

var joined = false
var username = ""
var socket = null

const nameToPct = (name) => {
    let hash = 0, chr
    for (let i = 0; i < name.length; i++) {
      chr = name.charCodeAt(i)
      hash = ((hash << 5) - hash) + chr
      hash |= 0
    }
    return hash % 100
}

const selectNextUrl = () => {
  if (isDev) {
    return `ws://localhost:8080/ws`
  }

  // Select one randomly
  return servers[Math.floor(Math.random() * servers.length)]
}

const sendJoin = () => {
  const request = { name: username, action: 'join' }
  console.log(request)
  socket.send(JSON.stringify(request))
}

const beginGameAtUrl = (url) => {
  const joinButton = document.getElementById('join')
  const canvas = document.getElementById('canvas')

  socket = new WebSocket(url)

  socket.addEventListener("open", (event) => {
    if (joined) {
      sendJoin()
    } else {
      joinButton.disabled = false
    }
  })

  socket.addEventListener('message', (event) => {
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
    canvas.className = `theme-${theme}`

    const children = []
    for (const player of state.players) {
      const pct = nameToPct(player.name)

      let imgUrl = `../sprites/${theme}/player.png`
      if (isDev) {
        // Workaround for the weirdest bug that I have seen:
        // File-system urls don't work in `mask`!
        imgUrl = `https://raw.githubusercontent.com/kofoednielsen/pass/main/frontend/sprites/${theme}/player.png`
      }

      const elem = document.createElement('div')
      children.push(elem)
      elem.className = `player`
      elem.style.webkitMask = `url("${imgUrl}")`
      elem.style.mask = `url("${imgUrl}")`
      elem.style.backgroundColor = `hsl(${pct} 100% 40%)`
      elem.style.gridColumnStart = player.position.x + 1
      elem.style.gridRowStart = player.position.y + 1
      const img = document.createElement('img')
      elem.appendChild(img)
      img.src = imgUrl
    }

    for (const proj of state.projectiles) {
      const elem = document.createElement('img')
      children.push(elem)
      elem.src = `../sprites/${theme}/projectile.png`
      elem.style.gridColumnStart = proj.x + 1
      elem.style.gridRowStart = proj.y + 1
    }

    canvas.replaceChildren(...children)
  })

  socket.addEventListener('error', (event) => {
    console.error("WebSocket error: ", event)
  })

  socket.addEventListener("close", () => {
    beginGameAtUrl(selectNextUrl())
  })
}

window.addEventListener("load", () => {
  const joinButton = document.getElementById('join')
  const usernameField = document.getElementById('username')
  usernameField.focus()

  const canvas = document.getElementById('canvas')
  canvas.style.gridTemplateColumns = `repeat(${width}, 20px)`
  canvas.style.gridTemplateRows = `repeat(${height}, 20px)`

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
          console.log({name: username, action})
          socket.send(JSON.stringify({name: username, action}))
        }
      }
  })
})
