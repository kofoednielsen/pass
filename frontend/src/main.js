window.addEventListener("load", function() {
  var joined = false
  var username = ""
  const join_button = document.getElementById('join')

  var socket = new WebSocket(`ws://${location.host}/snake`)

  socket.addEventListener("open", (event) => {
    join_button.disabled = false
  })

  socket.addEventListener('message', (event) => {
    const state = JSON.parse(event.data)
    const canvas = document.getElementById("canvas")
    let html = ""
    for (let y = 0; y < 20; y++) {
      for (let x = 0; x < 20; x++) {
        var object = "nothing"
        for (const proj of state.projectiles) {
          if (proj.x === x && proj.y === y) {
            object = "projectile"
          }
        }
        for (const player of state.players) {
          if (player.position.x === x && player.position.y === y) {
            object = "player"
          }
        }

        if (object === "nothing") {
          html += `<div></div>`
        } else {
          html += `<img src="/sprites/${state.theme}/${object}.png"></img>`
        }

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

  document.addEventListener('keydown', function(event) {
      if(event.key && joined) {
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
