import sys, json
from random import randint
from flask import Flask, request
import simple_websocket

MAP_SIZE = 20

app = Flask(__name__)

players = {}
wut = {"p": [
    {
        "x": randint(1, MAP_SIZE - 1),
        "y": randint(1, MAP_SIZE - 1),
        "vx": randint(-2, 2),
        "vy": randint(-2, 2),
        "ttl": 2000,
    }
]}


def get_state():
    return json.dumps({
        "event": "state",
        "theme": "star",
        "players": list(players.values()),
        "projectiles": wut["p"],
    })


DIRECTIONS = {"right": (1, 0), "left": (-1, 0), "up": (0, -1), "down": (0, 1)}


def process_move(ws):
    # Move projectiles
    for i in range(len(wut["p"])):
        try:
            wut["p"][i]["ttl"] -= 1
            if wut["p"][i]["ttl"] < 0:
                del wut["p"][i]
        except: pass
        else:
            wut["p"][i]["x"] += wut["p"][i]["vx"]
            wut["p"][i]["x"] %= MAP_SIZE
            wut["p"][i]["y"] += wut["p"][i]["vy"]
            wut["p"][i]["y"] %= MAP_SIZE

    # Check for collisions
    for name in list(players.keys()):
        pos = players[name]["position"]
        for i, proj in enumerate(wut["p"]):
            if proj["x"] == pos["x"] and proj["y"] == pos["y"]:
                # drop hitting projectile
                del wut["p"][i]
                players[name]["health"] -= (15 + randint(1, 20))
        if players[name]["health"] <= 0:
            ws.send(json.dumps({"event": "switch", "name": name}))
            del players[name]


@app.route("/star", websocket=True)
def f():
    ws = simple_websocket.Server(request.environ)
    while True:
        ws.send(get_state())
        process_move(ws)

        try:
            blob = ws.receive(timeout=0.2)
        except:
            continue
        if blob:
            data = json.loads(blob)
            print(f"Got data: {data}", file=sys.stderr)

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
            elif action == "attack":
                pos = players[name]["position"]
                for vx, vy in DIRECTIONS.values():
                    wut["p"].append({"x": pos["x"], "y": pos["y"], "vx": vx, "vy": vy, "ttl": 25})
            else:
                print(f"Bad event: {data}", file=sys.stderr)


@app.route("/")
def hello_world():
    return "<p>Hello, World!</p>"


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=80)
