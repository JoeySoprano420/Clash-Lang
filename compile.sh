python clashc.py input.clsh output.asm
nasm -f elf64 output.asm -o output.o
ld output.o -o output
./output
