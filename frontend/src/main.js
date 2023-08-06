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

const beginGameAtUrl = (url) => {
  var joined = false
  var username = ""
  const join_button = document.getElementById('join')

  var socket = new WebSocket(url)

  socket.addEventListener("open", (event) => {
    join_button.disabled = false
  })

  socket.addEventListener('message', (event) => {
    const state = JSON.parse(event.data)
    const canvas = document.getElementById("canvas")
    let html = ""
    for (let y = 0; y < 20; y++) {
      for (let x = 0; x < 20; x++) {
        var object = "<div></div>"
        for (const proj of state.projectiles) {
          if (proj.x === x && proj.y === y) {
            object = `<div><img src="../sprites/${state.theme}/projectile.png"></img></div>`
          }
        }
        for (const player of state.players) {
          if (player.position.x === x && player.position.y === y) {
            const pct = nameToPct(player.name)
            img= `../sprites/${state.theme}/player.png`
            object = `<div><div class="player-color" style="-webkit-mask: url('${img}'); background-color: hsl(${pct} 100% 40%)"><img src="${img}"></img></div></div>` }
        }
        html += object
      }
    }
    canvas.innerHTML = html
  })

  join_button.addEventListener('click', () => {
    const value = document.getElementById('username').value
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
