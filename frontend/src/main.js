const nameToPct = (name) => {
    var hash = 0, i, chr;
    for (i = 0; i < name.length; i++) {
      chr = name.charCodeAt(i);
      hash = ((hash << 5) - hash) + chr;
      hash |= 0;
    }
    return hash % 100;
}

const selectNextUrl = () => {
  if (location.host) {
    return `ws://${location.host}/snake`
  } else {
    // Allow testing locally when the host is not set (i.e. likely a file system path)
    return `ws://localhost:8080/ws`
  }
}

// First one here will be the fallback theme
const knownThemes = ['snake']

const beginGameAtUrl = (url) => {
  var joined = false
  var username = ""
  const joinButton = document.getElementById('join')
  const usernameField = document.getElementById('username')
  usernameField.focus()

  var socket = new WebSocket(url)

  socket.addEventListener("open", (event) => {
    joinButton.disabled = false
  })

  socket.addEventListener('message', (event) => {
    const state = JSON.parse(event.data)
    let theme
    if (knownThemes.indexOf(state.theme) === -1) {
      theme = knownThemes[0]
    } else {
      theme = state.theme
    }
    const canvas = document.getElementById('canvas')
    canvas.className = `theme-${theme}`
    canvas.style.gridTemplateColumns = `repeat(20, 20px)`
    canvas.style.gridTemplateRows = `repeat(20, 20px)`

    const children = []
    for (const player of state.players) {
      const pct = nameToPct(player.name)

      const elem = document.createElement('div')
      children.push(elem)
      elem.className = `player`
      elem.style.backgroundColor = `hsl(${pct} 100% 40%)`
      elem.style.gridColumnStart = player.position.x
      elem.style.gridRowStart = player.position.y
      const img = document.createElement('img')
      elem.appendChild(img)
      img.src = `../sprites/${theme}/player.png`
    }

    for (const proj of state.projectiles) {
      const elem = document.createElement('img')
      children.push(elem)
      elem.src = `../sprites/${theme}/projectile.png`
      elem.style.gridColumnStart = proj.x
      elem.style.gridRowStart = proj.y
    }

    canvas.replaceChildren(...children)
  })

  socket.addEventListener('error', (event) => {
    console.error("WebSocket error: ", event);
  })

  usernameField.addEventListener("keypress", function(event) {
    // If the user presses the "Enter" key on the keyboard
    if (event.key === "Enter") {
      event.preventDefault();
      joinButton.click();
    }
  });

  joinButton.addEventListener('click', () => {
    const value = usernameField.value
    if (value) {
      username = value
      const username_input = document.querySelector(".username-input")
      username_input.style.visibility = 'hidden'
      joined = true
      console.log({name: username, action: 'join'})
      socket.send(JSON.stringify({name: username, action: 'join'}))
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
}

window.addEventListener("load", () => {
  beginGameAtUrl(selectNextUrl())
})
