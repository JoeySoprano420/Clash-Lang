import os, json

def load_plugins():
    for fname in os.listdir("plugins"):
        if fname.endswith(".json"):
            with open(f"plugins/{fname}") as f:
                meta = json.load(f)
                print(f"🔌 Loaded plugin: {meta['plugin_name']} ({meta['version']})")
                print(f"📎 Entry file: {meta['entry']}")
load_plugins()
