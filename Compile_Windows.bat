nasm -f win64 output.asm -o output.obj
GoLink /console /entry _start output.obj kernel32.dll
output.exe
