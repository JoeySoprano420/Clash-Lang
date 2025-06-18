import os
def build_all():
    os.system("electron-builder --win --x64")  # .exe
    os.system("electron-builder --mac --x64")  # .dmg
    os.system("electron-builder --linux --deb")  # .deb
    print("📦 All platform installers built!")

if __name__ == "__main__":
    build_all()
