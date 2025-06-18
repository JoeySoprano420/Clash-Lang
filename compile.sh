python clashc.py input.clsh output.asm
nasm -f elf64 output.asm -o output.o
ld output.o -o output
./output

python clashc.py input.clsh
nasm -f elf64 input.asm -o input.o
nasm -f elf64 System.asm -o System.o
ld input.o System.o -o output
./output
