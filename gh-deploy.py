import os, shutil

def deploy_gh_pages():
    os.makedirs("gh-pages", exist_ok=True)
    shutil.copy("Clashup_Supreme.img", "gh-pages/Clashup_Supreme.img")

    with open("gh-pages/index.html", "w") as f:
        f.write("""
<!DOCTYPE html>
<html>
<head><title>Clashup Web VM</title><script src="https://unpkg.com/v86/build/libv86.js"></script></head>
<body><h1>🌀 Clashup Web Boot</h1><div id="screen" style="width:640px;height:400px;background:#000;"></div>
<script>
var emulator = new V86Starter({
  wasm_path: "https://unpkg.com/v86/build/v86.wasm",
  memory_size: 64 * 1024 * 1024,
  screen_container: document.getElementById("screen"),
  bios: { url: "https://unpkg.com/v86/build/seabios.bin" },
  vga_bios: { url: "https://unpkg.com/v86/build/vgabios.bin" },
  fda: { url: "Clashup_Supreme.img" },
  autostart: true
});
</script>
</body>
</html>
        """)

    print("✅ GitHub Pages files generated in ./gh-pages/")
    print("👉 Push to `gh-pages` branch to deploy")

deploy_gh_pages()
