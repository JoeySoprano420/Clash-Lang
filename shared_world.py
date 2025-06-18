import json
from flask import Flask, request

app = Flask(__name__)
world_state = {
    "campaign_phase": 1,
    "zones": {
        "Zone-1": {"status": "under attack"},
        "Zone-2": {"status": "safe"}
    }
}

@app.route("/state", methods=["GET"])
def get_state():
    return json.dumps(world_state)

@app.route("/state/update", methods=["POST"])
def update_state():
    data = request.json
    world_state["zones"].update(data.get("zones", {}))
    world_state["campaign_phase"] = data.get("campaign_phase", world_state["campaign_phase"])
    return {"ok": True}

app.run(port=9000)
