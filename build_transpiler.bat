@echo off
nasm -f win32 clash_transpiler_bootstrap.asm -o transpiler.obj
GoLink transpiler.obj kernel32.dll
echo [✓] Clash Transpiler Executable Ready: clash.exe
