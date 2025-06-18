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
