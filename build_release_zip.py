import zipfile, os
from datetime import datetime

def zip_dir(path, ziph):
    for root, dirs, files in os.walk(path):
        for file in files:
            ziph.write(os.path.join(root, file),
                       os.path.relpath(os.path.join(root, file), path))

def build_release(version="dev"):
    stamp = datetime.now().strftime("%Y%m%d")
    filename = f"Clashup_{version}_{stamp}.zip"
    with zipfile.ZipFile(filename, 'w', zipfile.ZIP_DEFLATED) as zipf:
        zip_dir("src", zipf)
        zip_dir("examples", zipf)
        zip_dir("bin", zipf)
        zip_dir("web", zipf)
        zipf.write("README.md")
        zipf.write("ClashupManual.pdf")
    print(f"📦 Release created: {filename}")

if __name__ == "__main__":
    build_release("v1.0")

python clashc.py myfile.clsh
nasm -f elf64 output.asm -o output.o
ld output.o -o output
