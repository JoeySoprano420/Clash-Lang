; clash.asm - Clash Language Compiler Runtime (Bootstrap)
BITS 32
ORG 0x400000

%include "win32n.inc"
%include "clash_std.asm"

section .text
global start
start:
    call    main
    call    ExitProcess

main:
    print message
    ret

section .data
message db "Hello from Clash!", 0

section .idata
; Import ExitProcess from kernel32
kernel32_dll db "kernel32.dll",0
ExitProcess_str db "ExitProcess",0

idt_start:
    dd 0,0,0,RVA kernel32_dll
    dd 0,0,0,0

idt_end:

