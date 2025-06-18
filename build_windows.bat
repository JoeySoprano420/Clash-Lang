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

build_windows.bat

@echo off
python clashc.py input.clsh
nasm -f win64 output.asm -o output.obj
GoLink /console /entry _start output.obj
output.exe


