import sys, json
from random import randint
from flask import Flask
from flask_sock import Sock

MAP_SIZE = 20

app = Flask(__name__)
sock = Sock(app)

players = {}
wut = {"p": []}


def get_state():
    return json.dumps({
        "event": "state",
        "theme": "star",
        "players": list(players.values()),
        "projectiles": wut["p"],
    })


DIRECTIONS = {"right": (1, 0), "left": (-1, 0), "up": (0, -1), "down": (0, 1)}


@sock.route("/star")
def f(ws):
    ws.send(get_state())
    while True:
        data = json.loads(ws.receive())
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
                wut["p"].append({"x": pos["x"], "y": pos["y"], "vx": vx, "vy": vy})
        else:
            print(f"Bad event: {data}", file=sys.stderr)

        # Move projectiles
        for i in range(len(wut["p"])):
            wut["p"][i]["x"] += wut["p"][i]["vx"]
            wut["p"][i]["x"] %= MAP_SIZE
            wut["p"][i]["y"] += wut["p"][i]["vy"]
            wut["p"][i]["y"] %= MAP_SIZE

        # Check for collisions
        pos = players[name]["position"]
        for proj in wut["p"]:
            if proj["x"] == pos["x"] and proj["y"] == pos["y"]:
                # drop hitting projectile
                wut["p"] = [
                    hmm
                    for hmm in wut["p"]
                    if hmm["x"] != proj["x"] and hmm["y"] != proj["y"]
                ]
                players[name]["health"] -= 10
                if players[name]["health"] <= 0:
                    ws.send(json.dumps({"event": "switch", "name": name}))

        ws.send(get_state())


@app.route("/")
def hello_world():
    return "<p>Hello, World!</p>"


if __name__ == "__main__":
    app.run(host="0.0.0.0", port=80, debug=True)
