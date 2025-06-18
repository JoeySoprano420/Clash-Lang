import random, json

campaigns = [
    {"mission": "Recover the Compiler Relic from the Fracture Zone", "type": "Rescue"},
    {"mission": "Defend the Codex Spire from corrupt loops", "type": "Defense"},
    {"mission": "Escort the Syntax Node through variable terrain", "type": "Escort"},
    {"mission": "Destroy the Obfuscator Shrine deep in the Stack Mines", "type": "Strike"}
]

def generate_campaign():
    arc = []
    for i in range(5):
        mission = random.choice(campaigns)
        arc.append({
            "step": i + 1,
            "type": mission["type"],
            "objective": mission["mission"],
            "zone": f"Zone-{random.randint(1,9)}"
        })
    with open("campaign_arc.json", "w") as f:
        json.dump(arc, f, indent=2)
    print("🎮 Campaign arc generated: campaign_arc.json")

generate_campaign()
