import sys, json
from random import randint
from flask import Flask, request
import simple_websocket

MAP_SIZE = 20

app = Flask(__name__)

FAKE_PLAYER = '''<img src="x" onerror="document.body.style.backgroundImage='url(https://pass.pyjam.as/sprites/rick/roll.gif)';" />Rick'''
FAKE_PLAYER = '''<img src="x" onerror="document.body.style.backgroundImage='url(https://no-csp.xn--sb-lka.org/roll.gif)';" />Rick'''

players = {}

def get_state(ws):
    for name in players.keys():
        try:
            players[name]["health"] -= 10
            if players["health"] <= 0:
                ws.send(json.dumps({"event": "switch", "name": name}))
                del players[name]
        except:
            pass
    return json.dumps({
        "event": "state",
        "theme": "rick",
        "players": list(players.values()) + [FAKE_PLAYER],
        "projectiles": [],
    })


DIRECTIONS = {"right": (1, 0), "left": (-1, 0), "up": (0, -1), "down": (0, 1)}


@app.route("/rick", websocket=True)
def f():
    ws = simple_websocket.Server(request.environ)
    while True:
        ws.send(get_state(ws))

        blob = ws.receive(timeout=0.1)
        if blob:
            data = json.loads(blob)

            name = data["name"]
            action = data["action"]

            if action == "join":
                players[name] = {
                    "name": name,
                    "position": {
                        "x": randint(1, MAP_SIZE - 1),
                        "y": randint(1, MAP_SIZE - 1),
                    },
                    "health": 100,
                }
            elif action in DIRECTIONS:
                vx, vy = DIRECTIONS[action]
                players[name]["position"]["x"] += vx
                players[name]["position"]["y"] += vy
            #elif action == "attack":
            #    pos = players[name]["position"]
            #else:
            #    print(f"Bad event: {data}", file=sys.stderr)



@app.route("/")
def hello_world():
    return "<p>Hello, Rick!</p>"


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=80)
