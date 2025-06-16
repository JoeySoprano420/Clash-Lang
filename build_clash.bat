@echo off
nasm -f win32 clash.asm -o clash.obj
GoLink clash.obj kernel32.dll
echo Clash Executable Ready: clash.exe

@echo off
nasm -f win32 clash_bootstrap.asm -o clash.obj
GoLink clash.obj kernel32.dll
echo Clash Compiler Ready! Output: clash.exe

