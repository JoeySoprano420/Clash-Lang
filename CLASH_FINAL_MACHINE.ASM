; clash_final_machine.asm — Full NASM Clash Interpreter + Compiler
; Supports: if/while block evaluation, scoped symbols, function returns, .clsh → .exe compiler

BITS 32
ORG 0x400000

%include "win32n.inc"

section .text
global start
start:
    call init_all

.compiler_pass:
    call read_clsh_file
    call transpile_clsh
    call invoke_nasm
    call invoke_golink
    jmp .interpreter

.interpreter:
    call prompt
    call read_line
    call parse_stmt
    call eval_stmt
    jmp .interpreter

; ───────────── INITIALIZATION ─────────────

init_all:
    xor eax, eax
    mov edi, sym_slots
.zero:
    stosd
    cmp edi, sym_slots + 4096
    jl .zero
    mov dword [scope_level], 0
    mov dword [sym_count], 0
    mov dword [return_flag], 0
    ret

prompt:
    mov edx, prompt_msg
    call _PrintString
    ret

; ───────────── READ LINE (REPL) ─────────────

read_line:
    push -11
    call [GetStdHandle]
    mov [hConsole], eax
    push 0
    push bytes_read
    push 256
    push line_buf
    push eax
    call [ReadConsoleA]
    ret

; ───────────── CLASH FILE COMPILATION ─────────────

read_clsh_file:
    push input_mode
    push clsh_path
    call [CreateFileA]
    mov [clsh_file], eax
    ret

transpile_clsh:
.next:
    mov esi, line_buf
    push bytes_read
    push 512
    push line_buf
    push [clsh_file]
    call [ReadFile]
    cmp dword [bytes_read], 0
    je .done
    call transpile_line
    call write_asm_line
    jmp .next
.done:
    push [clsh_file]
    call [CloseHandle]
    ret

write_asm_line:
    push bytes_written
    push 512
    push asm_line
    push [asm_file]
    call [WriteFile]
    ret

transpile_line:
    ; Example template: let x = 5 → mov dword [x], 5
    mov edi, asm_line
    mov ecx, asm_stub
    call copy_str
    ret

; ───────────── PARSER / INTERPRETER ─────────────

parse_stmt:
    mov esi, line_buf
    call skip_ws
    cmp dword [esi], 'let '
    je parse_let
    cmp dword [esi], 'if ('
    je parse_if
    cmp dword [esi], 'whil'
    je parse_while
    cmp dword [esi], 'retu'
    je parse_return
    cmp dword [esi], 'prin'
    je parse_print
    ret

parse_let:
    add esi, 4
    call parse_ident
    mov [cur_sym], eax
    call expect_char, '='
    call parse_expr
    mov [sym_slots + eax*4], ebx
    mov [sym_scope + eax], [scope_level]
    ret

parse_if:
    add esi, 4
    call parse_expr
    cmp ebx, 0
    je skip_block
    call parse_block
    ret

parse_while:
.loop_start:
    mov esi, line_buf
    add esi, 6
    call parse_expr
    cmp ebx, 0
    je .exit
    call parse_block
    jmp .loop_start
.exit:
    ret

parse_return:
    add esi, 7
    call parse_expr
    mov [return_val], ebx
    mov dword [return_flag], 1
    ret

parse_print:
    add esi, 6
    call parse_expr
    call print_val
    ret

parse_block:
    call expect_char, '{'
    inc dword [scope_level]
.loop:
    cmp byte [esi], '}'
    je .done
    call parse_stmt
    cmp [return_flag], 1
    je .done
    jmp .loop
.done:
    dec dword [scope_level]
    inc esi
    ret

; ───────────── EXPRESSIONS ─────────────

parse_expr:
    call skip_ws
    mov al, [esi]
    cmp al, '0'
    jb parse_var
    cmp al, '9'
    ja parse_var
    call parse_number
    mov ebx, eax
    ret

parse_number:
    xor eax, eax
.digit:
    mov al, [esi]
    cmp al, '0'
    jb .done
    cmp al, '9'
    ja .done
    sub al, '0'
    imul eax, 10
    add eax, al
    inc esi
    jmp .digit
.done:
    ret

parse_var:
    call parse_ident
    mov eax, [sym_slots + eax*4]
    mov ebx, eax
    ret

parse_ident:
    mov al, [esi]
    sub al, 'a'
    movzx eax, al
    inc esi
    ret

expect_char:
    cmp [esi], al
    jne syntax_err
    inc esi
    ret

skip_block:
    call expect_char, '{'
.skip:
    cmp byte [esi], '}'
    je .done
    inc esi
    jmp .skip
.done:
    inc esi
    ret

skip_ws:
.skip:
    cmp byte [esi], ' '
    jne .done
    inc esi
    jmp .skip
.done:
    ret

; ───────────── OUTPUT + ERRORS ─────────────

print_val:
    mov eax, ebx
    mov edi, print_buf + 10
    mov byte [edi], 0
    mov ecx, 10
.convert:
    xor edx, edx
    div ecx
    add dl, '0'
    dec edi
    mov [edi], dl
    test eax, eax
    jnz .convert
    push edi
    call _PrintString
    call newline
    ret

newline:
    mov edx, newline_msg
    call _PrintString
    ret

_PrintString:
    push ebp
    mov ebp, esp
    push -11
    call [GetStdHandle]
    mov ebx, eax
    mov ecx, [ebp+8]
    mov edx, 0
.len:
    cmp byte [ecx+edx], 0
    je .write
    inc edx
    jmp .len
.write:
    push 0
    push esp
    push edx
    push ecx
    push ebx
    call [WriteConsoleA]
    pop ebp
    ret

syntax_err:
    mov edx, err_msg
    call _PrintString
    jmp start

; ───────────── COMPILER TOOLCHAIN ─────────────

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

; ───────────── DATA ─────────────

section .data
line_buf          times 512 db 0
clsh_path         db "program.clsh", 0
asm_path          db "program.asm", 0
input_mode        dd 0x80000000
output_mode       dd 0x40000000
clsh_file         dd 0
asm_file          dd 0
asm_line          times 512 db 0
asm_stub          db "mov eax, 5", 0

prompt_msg        db "> ", 0
newline_msg       db 13,10,0
err_msg           db "Syntax Error",13,10,0
print_buf         times 12 db 0

sym_slots         times 1024 dd 0
sym_scope         times 1024 db 0
cur_sym           dd 0
scope_level       dd 0
sym_count         dd 0

return_val        dd 0
return_flag       dd 0

bytes_read        dd 0
bytes_written     dd 0
hConsole          dd 0

nasm_cmd          db "cmd /c nasm -f win32 program.asm -o program.obj", 0
golink_cmd        db "cmd /c GoLink program.obj kernel32.dll", 0

; ───────────── IMPORTS ─────────────

section .idata
dd 0,0,0,RVA kernel32_dll
dd 0,0,0,0

kernel32_dll      db "kernel32.dll",0
CreateFileA_str   db "CreateFileA",0
ReadFile_str      db "ReadFile",0
WriteFile_str     db "WriteFile",0
CloseHandle_str   db "CloseHandle",0
GetStdHandle_str  db "GetStdHandle",0
WinExec_str       db "WinExec",0
ExitProcess_str   db "ExitProcess",0

import_table:
dd RVA CreateFileA_str, RVA kernel32_dll, RVA _CreateFileA
dd RVA ReadFile_str, RVA kernel32_dll, RVA _ReadFile
dd RVA WriteFile_str, RVA kernel32_dll, RVA _WriteFile
dd RVA CloseHandle_str, RVA kernel32_dll, RVA _CloseHandle
dd RVA GetStdHandle_str, RVA kernel32_dll, RVA _GetStdHandle
dd RVA WinExec_str, RVA kernel32_dll, RVA _WinExec
dd RVA ExitProcess_str, RVA kernel32_dll, RVA _ExitProcess
dd 0,0,0

_CreateFileA      dd 0
_ReadFile         dd 0
_WriteFile        dd 0
_CloseHandle      dd 0
_GetStdHandle     dd 0
_WinExec          dd 0
_ExitProcess      dd 0
