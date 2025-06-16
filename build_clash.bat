@echo off
nasm -f win32 clash.asm -o clash.obj
GoLink clash.obj kernel32.dll
echo Clash Executable Ready: clash.exe
