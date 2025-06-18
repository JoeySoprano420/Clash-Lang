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
