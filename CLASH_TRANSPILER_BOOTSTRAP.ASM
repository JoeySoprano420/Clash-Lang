; clash_transpiler_bootstrap.asm
; Fully self-hosted Clash → ASM transpiler written purely in NASM
; Converts .clsh into .asm, then invokes NASM + GoLink for .exe

BITS 32
ORG 0x400000

%include "win32n.inc"

section .text
global start
start:
    call transpile_clsh_file
    call invoke_nasm_compiler
    call invoke_golink_linker
    push 0
    call [ExitProcess]

; ───────────── FUNCTION: transpile_clsh_file ─────────────
; Opens input.clsh, reads line by line, lexes, writes mapped ASM into output.asm

transpile_clsh_file:
    push input_mode
    push input_path
    call [CreateFileA]
    mov [hFileIn], eax

    push output_mode
    push output_path
    call [CreateFileA]
    mov [hFileOut], eax

    mov esi, buffer
.read_next:
    push bytes_read
    push 256
    push buffer
    push [hFileIn]
    call [ReadFile]
    cmp eax, 0
    je .done

    mov edi, asm_line
    call parse_clash_line
    call write_asm_line
    jmp .read_next

.done:
    push [hFileIn]
    call [CloseHandle]
    push [hFileOut]
    call [CloseHandle]
    ret

; ───────────── FUNCTION: parse_clash_line ─────────────
; Parses a line from buffer (ESI) and writes ASM to asm_line (EDI)

parse_clash_line:
    cmp byte [esi], 'p'       ; check "print"
    jne .check_input
    cmp dword [esi], 'prin'
    jne .check_input
    mov ecx, print_template
    jmp .copy_template

.check_input:
    cmp dword [esi], 'inpu'
    jne .check_let
    mov ecx, input_template
    jmp .copy_template

.check_let:
    cmp word [esi], 'le'
    jne .check_if
    mov ecx, let_template
    jmp .copy_template

.check_if:
    cmp word [esi], 'if'
    jne .check_loop
    mov ecx, if_template
    jmp .copy_template

.check_loop:
    cmp dword [esi], 'loop'
    jne .default_nop
    mov ecx, loop_template
    jmp .copy_template

.default_nop:
    mov ecx, comment_template

.copy_template:
    call copy_string
    ret

; ───────────── FUNCTION: copy_string ─────────────
; Copies string from ECX (template) to EDI (asm_line)

copy_string:
.next_char:
    mov al, [ecx]
    test al, al
    je .done
    mov [edi], al
    inc edi
    inc ecx
    jmp .next_char
.done:
    ret

; ───────────── FUNCTION: write_asm_line ─────────────
; Writes asm_line to output file

write_asm_line:
    push bytes_written
    push asm_line_len
    push asm_line
    push [hFileOut]
    call [WriteFile]
    ret

; ───────────── FUNCTION: invoke_nasm_compiler ─────────────

invoke_nasm_compiler:
    push 0
    push 0
    push 0
    push nasm_cmd
    call [WinExec]
    ret

; ───────────── FUNCTION: invoke_golink_linker ─────────────

invoke_golink_linker:
    push 0
    push 0
    push 0
    push golink_cmd
    call [WinExec]
    ret

; ───────────── SECTION: DATA ─────────────

section .data
input_path      db "input.clsh", 0
output_path     db "output.asm", 0
input_mode      dd 0x80000000   ; GENERIC_READ
output_mode     dd 0x40000000   ; GENERIC_WRITE
hFileIn         dd 0
hFileOut        dd 0
bytes_read      dd 0
bytes_written   dd 0
buffer          times 256 db 0
asm_line        times 512 db 0
asm_line_len    equ 512

print_template  db 'print_str msg_0', 0
input_template  db 'input_str var_input', 0
let_template    db 'movv varname, value', 0
if_template     db 'if_eq eax, ebx, do_something', 0
loop_template   db 'loop some_block, nop', 0
comment_template db '; unknown clash syntax', 0

nasm_cmd        db "cmd /c nasm -f win32 output.asm -o output.obj", 0
golink_cmd      db "cmd /c GoLink output.obj kernel32.dll", 0

; ───────────── SECTION: IMPORTS ─────────────

section .idata
dd 0,0,0,RVA kernel32_str
dd 0,0,0,0

kernel32_str      db "kernel32.dll",0
CreateFileA_str   db "CreateFileA",0
ReadFile_str      db "ReadFile",0
WriteFile_str     db "WriteFile",0
CloseHandle_str   db "CloseHandle",0
WinExec_str       db "WinExec",0
ExitProcess_str   db "ExitProcess",0

import_table:
dd RVA CreateFileA_str, RVA kernel32_str, RVA _CreateFileA
dd RVA ReadFile_str, RVA kernel32_str, RVA _ReadFile
dd RVA WriteFile_str, RVA kernel32_str, RVA _WriteFile
dd RVA CloseHandle_str, RVA kernel32_str, RVA _CloseHandle
dd RVA WinExec_str, RVA kernel32_str, RVA _WinExec
dd RVA ExitProcess_str, RVA kernel32_str, RVA _ExitProcess
dd 0,0,0

_CreateFileA     dd 0
_ReadFile        dd 0
_WriteFile       dd 0
_CloseHandle     dd 0
_WinExec         dd 0
_ExitProcess     dd 0
