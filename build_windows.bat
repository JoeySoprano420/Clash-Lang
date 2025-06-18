@echo off
python clashc.py input.clsh output.asm
nasm -f win64 output.asm -o output.obj
GoLink /console /entry _start output.obj
echo Done. Running output.exe...
output.exe

@echo off
python clashc.py input.clsh output.asm
nasm -f win64 output.asm -o output.obj
GoLink /console /entry _start output.obj
output.exe

@echo off
python clashc.py input.clsh
nasm -f win64 output.asm -o output.obj
GoLink /console /entry _start output.obj
output.exe

build_windows.bat

@echo off
clashup_exec.exe

@echo off
echo Setting up Clashup on Windows...
python clashc.py input.clsh
nasm -f win64 input.asm -o input.obj
GoLink.exe /console input.obj System.obj
echo Done! Output is: input.exe
pause

install.bat

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

@echo off
nasm -f win32 clash_transpiler_bootstrap.asm -o transpiler.obj
GoLink transpiler.obj kernel32.dll
echo [✓] Clash Transpiler Executable Ready: clash.exe

@echo off
python src\clashc\main.py examples\hello.clsh
nasm -f win64 bin\output.asm -o bin\output.obj
GoLink /console /entry _start bin\output.obj
clashup_exec.exe

build_windows.bat

cd ClashupInstaller
install.bat
