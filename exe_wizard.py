import subprocess, shutil, os

def build_exe(splash_img="splash.bmp", icon_ico="icon.ico"):
    print("🔧 Compiling Clashup .clsh to .exe...")

    # Step 1: Compile to NASM
    subprocess.run(["python", "clashc.py", "input.clsh"])
    subprocess.run(["nasm", "-f", "win64", "output.asm", "-o", "output.o"])
    subprocess.run(["GoLink.exe", "/console", "output.o", "System.asm", f"/icon:{icon_ico}"])

    # Step 2: Splash embed (for systems with resource hacker or compatible tools)
    if os.path.exists("GoRC.exe") and os.path.exists("GoLink.exe"):
        with open("splash.rc", "w") as f:
            f.write(f'splash BITMAP "{splash_img}"\n')
        subprocess.run(["GoRC.exe", "splash.rc"])
        subprocess.run(["GoLink.exe", "output.o", "System.asm", "splash.res", f"/icon:{icon_ico}"])

    # Move to /build
    os.makedirs("build", exist_ok=True)
    shutil.move("output.exe", "build/clashup_custom.exe")
    print("✅ clashup_custom.exe created in /build/")

if __name__ == "__main__":
    build_exe()
