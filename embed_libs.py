import os, shutil, re

def parse_imports(filename="input.clsh"):
    with open(filename, 'r') as f:
        code = f.read()
    return re.findall(r'import\s+"(.*?)"', code)

def embed_deps():
    deps = parse_imports()
    os.makedirs("build/clshlibs", exist_ok=True)
    for dep in deps:
        lib_file = f"clshlibs/{dep}"
        if os.path.exists(lib_file):
            shutil.copy(lib_file, f"build/clshlibs/{dep}")
            print(f"📎 Embedded {dep}")
        else:
            print(f"⚠️ Missing lib: {dep}")

if __name__ == "__main__":
    embed_deps()
