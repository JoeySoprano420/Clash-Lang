import os, shutil, json

PKG_DB = "clpm.json"
LIB_DIR = "clshlibs"

def init_db():
    if not os.path.exists(PKG_DB):
        with open(PKG_DB, 'w') as f:
            json.dump({}, f)
    if not os.path.exists(LIB_DIR):
        os.makedirs(LIB_DIR)

def list_pkgs():
    with open(PKG_DB) as f:
        db = json.load(f)
    for name, meta in db.items():
        print(f"{name} - v{meta['version']}")

def install_pkg(name, source_path):
    init_db()
    with open(PKG_DB) as f:
        db = json.load(f)
    shutil.copy(source_path, f"{LIB_DIR}/{name}.clshlib")
    db[name] = {"version": "1.0", "source": source_path}
    with open(PKG_DB, 'w') as f:
        json.dump(db, f, indent=2)
    print(f"✅ Installed {name} from {source_path}")

def remove_pkg(name):
    init_db()
    with open(PKG_DB) as f:
        db = json.load(f)
    if name in db:
        os.remove(f"{LIB_DIR}/{name}.clshlib")
        del db[name]
        with open(PKG_DB, 'w') as f:
            json.dump(db, f, indent=2)
        print(f"🗑 Removed {name}")
    else:
        print("❌ Package not found")

# CLI Mode
if __name__ == "__main__":
    import sys
    cmd = sys.argv[1] if len(sys.argv) > 1 else "help"
    if cmd == "list":
        list_pkgs()
    elif cmd == "install":
        install_pkg(sys.argv[2], sys.argv[3])
    elif cmd == "remove":
        remove_pkg(sys.argv[2])
    else:
        print("CLPM Commands:\n  list\n  install <name> <source>\n  remove <name>")

import os, shutil, json, urllib.request

PKG_DB = "clpm.json"
LIB_DIR = "clshlibs"

def init_db():
    os.makedirs(LIB_DIR, exist_ok=True)
    if not os.path.exists(PKG_DB):
        with open(PKG_DB, 'w') as f:
            json.dump({}, f)

def fetch_remote_pkg(name, url, version="1.0"):
    init_db()
    path = f"{LIB_DIR}/{name}.clshlib"
    try:
        urllib.request.urlretrieve(url, path)
        with open(PKG_DB) as f:
            db = json.load(f)
        db[name] = {"version": version, "source": url}
        with open(PKG_DB, 'w') as f:
            json.dump(db, f, indent=2)
        print(f"📦 {name}@{version} downloaded successfully.")
    except Exception as e:
        print(f"❌ Failed to fetch {name}: {e}")

# CLI
if __name__ == "__main__":
    import sys
    cmd = sys.argv[1] if len(sys.argv) > 1 else "help"
    if cmd == "fetch":
        fetch_remote_pkg(sys.argv[2], sys.argv[3], sys.argv[4] if len(sys.argv) > 4 else "1.0")
    else:
        print("Usage: fetch <name> <url> [version]")

