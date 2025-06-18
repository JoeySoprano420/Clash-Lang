import shutil, os

def build_suite():
    os.makedirs("Clashup_Suite", exist_ok=True)
    files = [
        "clashc.py", "clpm.py", "gui_launcher.py", "playground.py",
        "clashup_tutor.py", "manual_gen.py", "System.asm",
        "install.sh", "install.bat", "input.clsh", "README.md"
    ]
    for file in files:
        shutil.copy(file, f"Clashup_Suite/{file}")
    shutil.copytree("clshlibs", "Clashup_Suite/clshlibs", dirs_exist_ok=True)
    print("✅ Clashup Suite built in ./Clashup_Suite")

if __name__ == "__main__":
    build_suite()
