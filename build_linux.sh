#!/bin/bash
python3 clashc.py input.clsh output.asm
nasm -f elf64 output.asm -o output.o
ld output.o -o output
chmod +x output
echo "🎉 Built Linux binary: ./output"
./output

#!/bin/bash
python3 clashc.py input.clsh output.asm
nasm -f elf64 output.asm -o output.o
ld output.o -o output
chmod +x output
./output

./build_linux.sh

Clashup/build_linux.sh

chmod +x build_linux.sh
./build_linux.sh

#!/bin/bash
./clashup_exec
