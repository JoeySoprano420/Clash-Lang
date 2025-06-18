import requests

def sync_campaign():
    state = requests.get("http://localhost:9000/state").json()
    print(f"🌍 Current Campaign Phase: {state['campaign_phase']}")
    for z, status in state["zones"].items():
        print(f"  - {z}: {status}")
