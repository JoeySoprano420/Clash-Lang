@echo off
nasm -f win32 clash.asm -o clash.obj
GoLink clash.obj kernel32.dll
echo Clash Executable Ready: clash.exe

@echo off
nasm -f win32 clash_bootstrap.asm -o clash.obj
GoLink clash.obj kernel32.dll
echo Clash Compiler Ready! Output: clash.exe

@echo off
nasm -f win32 clash_exec_forge_full.asm -o clash.obj
GoLink clash.obj kernel32.dll
clash.exe

#!/bin/bash
nasm -f elf clash_exec_forge_full.asm -o clash.o
ld -m elf_i386 -s -o clash clash.o

