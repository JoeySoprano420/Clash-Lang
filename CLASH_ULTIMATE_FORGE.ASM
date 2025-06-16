; clash_ultimate_forge.asm
; Fully monolithic NASM implementation:
; ✅ Infix → Postfix (shunting yard)
; ✅ Arg parsing with commas
; ✅ .clsh → .asm writer → .exe compiler
; By: YOU + NASM

BITS 32
ORG 0x400000

%include "win32n.inc"

section .text
global start
start:
    call init_runtime
    call read_clsh_file
    call transpile_all
    call invoke_nasm
    call invoke_golink
    jmp halt

; ───────────── INIT ─────────────

init_runtime:
    xor eax, eax
    mov edi, sym_table
.fill:
    stosd
    cmp edi, sym_table + 4096
    jl .fill
    mov dword [scope_depth], 0
    ret

; ───────────── .CLSH PARSER + .ASM EMITTER ─────────────

read_clsh_file:
    push input_mode
    push clsh_path
    call [CreateFileA]
    mov [clsh_handle], eax
    push output_mode
    push asm_path
    call [CreateFileA]
    mov [asm_handle], eax
    ret

transpile_all:
.next:
    mov esi, source_buf
    push read_bytes
    push 256
    push source_buf
    push [clsh_handle]
    call [ReadFile]
    cmp eax, 0
    je .done
    call parse_line
    call write_asm_line
    jmp .next
.done:
    push [clsh_handle]
    call [CloseHandle]
    push [asm_handle]
    call [CloseHandle]
    ret

write_asm_line:
    push bytes_written
    push 256
    push asm_buf
    push [asm_handle]
    call [WriteFile]
    ret

parse_line:
    ; Example: let x = 1 + 2 * 3 → postfix
    mov edi, postfix_buf
    mov esi, source_buf
    call shunting_yard
    call emit_asm_from_postfix
    ret

; ───────────── SHUNTING YARD (INFIX → POSTFIX) ─────────────

shunting_yard:
    xor ecx, ecx ; op stack ptr
.next_token:
    lodsb
    cmp al, 0
    je .end
    cmp al, '0'
    jb .check_op
    cmp al, '9'
    ja .check_op
    sub al, '0'
    stosb
    jmp .next_token
.check_op:
    cmp al, '+'
    je .op_add
    cmp al, '*'
    je .op_mul
    jmp .next_token
.op_add:
    cmp byte [op_stack + ecx - 1], '*'
    je .pop_stack
    mov [op_stack + ecx], al
    inc ecx
    jmp .next_token
.op_mul:
    mov [op_stack + ecx], al
    inc ecx
    jmp .next_token
.pop_stack:
    dec ecx
    mov al, [op_stack + ecx]
    stosb
    jmp .op_add
.end:
    ; Flush remaining ops
    dec ecx
.loop:
    cmp ecx, -1
    jl .done
    mov al, [op_stack + ecx]
    stosb
    dec ecx
    jmp .loop
.done:
    ret

; ───────────── POSTFIX EMITTER → ASM ─────────────

emit_asm_from_postfix:
    mov esi, postfix_buf
    mov edi, asm_buf
.loop:
    lodsb
    cmp al, 0
    je .done
    cmp al, '+'
    je .add
    cmp al, '*'
    je .mul
    add al, '0'
    stosb
    stosb
    jmp .loop
.add:
    mov ecx, asm_add
    call copy_str
    jmp .loop
.mul:
    mov ecx, asm_mul
    call copy_str
    jmp .loop
.done:
    ret

; ───────────── ARGUMENT PARSING (FUNC x, y) ─────────────

parse_args:
    ; Reads x, y, z → stores in arg_list
    xor ecx, ecx
.read:
    call parse_ident
    mov [arg_list + ecx*4], eax
    inc ecx
    cmp byte [esi], ','
    jne .done
    inc esi
    jmp .read
.done:
    mov [arg_count], ecx
    ret

parse_ident:
    mov al, [esi]
    sub al, 'a'
    movzx eax, al
    inc esi
    ret

; ───────────── BUILD SYSTEM ─────────────

invoke_nasm:
    push 0
    push 0
    push 0
    push nasm_cmd
    call [WinExec]
    ret

invoke_golink:
    push 0
    push 0
    push 0
    push golink_cmd
    call [WinExec]
    ret

halt:
    push 0
    call [ExitProcess]

; ───────────── UTILITIES ─────────────

copy_str:
.loop:
    mov al, [ecx]
    test al, al
    je .done
    stosb
    inc ecx
    jmp .loop
.done:
    ret

; ───────────── DATA ─────────────

section .data
clsh_path       db "main.clsh",0
asm_path        db "main.asm",0
input_mode      dd 0x80000000
output_mode     dd 0x40000000
clsh_handle     dd 0
asm_handle      dd 0

source_buf      times 256 db 0
asm_buf         times 256 db 0
postfix_buf     times 256 db 0
op_stack        times 32  db 0

sym_table       times 1024 dd 0
arg_list        times 16   dd 0
arg_count       dd 0
scope_depth     dd 0

bytes_written   dd 0
read_bytes      dd 0

asm_add         db "ADD EAX, EBX",0
asm_mul         db "IMUL EAX, EBX",0

prompt_msg      db "> ",0
newline_msg     db 13,10,0

nasm_cmd        db "cmd /c nasm -f win32 main.asm -o main.obj", 0
golink_cmd      db "cmd /c GoLink main.obj kernel32.dll", 0

; ───────────── IMPORTS ─────────────

section .idata
dd 0,0,0,RVA kernel32_dll
dd 0,0,0,0

kernel32_dll        db "kernel32.dll",0
CreateFileA_str     db "CreateFileA",0
ReadFile_str        db "ReadFile",0
WriteFile_str       db "WriteFile",0
CloseHandle_str     db "CloseHandle",0
GetStdHandle_str    db "GetStdHandle",0
WinExec_str         db "WinExec",0
ExitProcess_str     db "ExitProcess",0

import_table:
dd RVA CreateFileA_str, RVA kernel32_dll, RVA _CreateFileA
dd RVA ReadFile_str, RVA kernel32_dll, RVA _ReadFile
dd RVA WriteFile_str, RVA kernel32_dll, RVA _WriteFile
dd RVA CloseHandle_str, RVA kernel32_dll, RVA _CloseHandle
dd RVA GetStdHandle_str, RVA kernel32_dll, RVA _GetStdHandle
dd RVA WinExec_str, RVA kernel32_dll, RVA _WinExec
dd RVA ExitProcess_str, RVA kernel32_dll, RVA _ExitProcess
dd 0,0,0

_CreateFileA        dd 0
_ReadFile           dd 0
_WriteFile          dd 0
_CloseHandle        dd 0
_GetStdHandle       dd 0
_WinExec            dd 0
_ExitProcess        dd 0
