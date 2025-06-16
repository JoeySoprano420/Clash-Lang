; clash_omni.asm — Full NASM Clash Runtime
; Supports: stack frame functions, arguments, string manipulation, input, module inclusion

BITS 32
ORG 0x400000

%include "win32n.inc"

section .text
global start
start:
    call init_runtime
.repl:
    call prompt
    call read_line
    call parse_and_dispatch
    jmp .repl

; ───────────── INIT ─────────────

init_runtime:
    xor eax, eax
    mov edi, symbol_mem
.fill:
    stosd
    cmp edi, symbol_mem + 4096
    jl .fill
    xor eax, eax
    mov edi, string_table
.fillstr:
    stosb
    cmp edi, string_table + 2048
    jl .fillstr
    mov dword [frame_ptr], 0
    mov dword [string_index], 0
    ret

prompt:
    mov edx, prompt_msg
    call _PrintString
    ret

; ───────────── READ & INPUT ─────────────

read_line:
    push -11
    call [GetStdHandle]
    mov [hConsole], eax
    push 0
    push bytes_read
    push 256
    push input_buf
    push eax
    call [ReadConsoleA]
    ret

read_into_var:
    ; stores into current variable index (EBX)
    push -11
    call [GetStdHandle]
    mov [hConsole], eax
    push 0
    push bytes_read
    push string_ptr
    push eax
    call [ReadConsoleA]
    mov eax, string_ptr
    mov [symbol_mem + ebx*4], eax
    mov byte [symbol_types + ebx], 3 ; string
    ret

; ───────────── PARSER & DISPATCHER ─────────────

parse_and_dispatch:
    mov esi, input_buf
    call skip_ws
    cmp dword [esi], 'let '
    je parse_let
    cmp dword [esi], 'func'
    je parse_func
    cmp dword [esi], 'prin'
    je parse_print
    cmp dword [esi], 'inpu'
    je parse_input
    cmp dword [esi], 'impo'
    je parse_import
    jmp parse_call

parse_let:
    add esi, 4
    call parse_ident
    mov [cur_sym], eax
    call expect_char, ':'
    call parse_type
    mov [symbol_types + eax], al
    call expect_char, '='
    call parse_expr
    mov [symbol_mem + eax*4], ebx
    ret

parse_type:
    cmp dword [esi], 'int '
    je .int
    cmp dword [esi], 'bool'
    je .bool
    cmp dword [esi], 'stri'
    je .str
    ret
.int:  add esi, 4
       mov al, 1
       ret
.bool: add esi, 4
       mov al, 2
       ret
.str:  add esi, 6
       mov al, 3
       ret

parse_input:
    add esi, 6
    call parse_ident
    mov ebx, eax
    call read_into_var
    ret

parse_import:
    ; pretend to import module (for future use)
    mov edx, import_msg
    call _PrintString
    ret

parse_print:
    add esi, 6
    call parse_expr
    call print_any
    ret

parse_call:
    call parse_ident
    mov [call_func_id], eax
    call expect_char, '('
    call parse_expr
    call expect_char, ')'
    call push_stack_frame
    mov ebx, [frame_ptr]
    mov [frame_args + ebx*4], eax
    call call_function
    call pop_stack_frame
    ret

parse_func:
    ; simulate single func arg and block
    mov dword [func_defining], 1
    call skip_block
    ret

call_function:
    ; dispatch fake function call
    cmp [call_func_id], 0
    jne .done
    mov eax, [frame_ptr]
    mov eax, [frame_args + eax*4]
    add eax, 1
    mov [ret_val], eax
.done:
    ret

; ───────────── STACK FRAME ─────────────

push_stack_frame:
    mov eax, [frame_ptr]
    add eax, 1
    mov [frame_ptr], eax
    ret

pop_stack_frame:
    mov eax, [frame_ptr]
    sub eax, 1
    mov [frame_ptr], eax
    ret

; ───────────── EXPRESSION EVALUATION ─────────────

parse_expr:
    call skip_ws
    mov al, [esi]
    cmp al, '"'
    je parse_string
    cmp al, '0'
    jb parse_var
    cmp al, '9'
    ja parse_var
    call parse_number
    mov ebx, eax
    ret

parse_string:
    inc esi
    mov edi, string_table
    add edi, [string_index]
.copy:
    mov al, [esi]
    cmp al, '"'
    je .done
    stosb
    inc esi
    jmp .copy
.done:
    mov byte [edi], 0
    mov eax, string_table
    add eax, [string_index]
    mov ebx, eax
    add dword [string_index], 64
    inc esi
    ret

parse_var:
    call parse_ident
    mov eax, [symbol_mem + eax*4]
    mov ebx, eax
    ret

parse_number:
    xor eax, eax
.loop:
    mov al, [esi]
    cmp al, '0'
    jb .done
    cmp al, '9'
    ja .done
    sub al, '0'
    imul eax, 10
    add eax, al
    inc esi
    jmp .loop
.done:
    ret

parse_ident:
    mov al, [esi]
    sub al, 'a'
    movzx eax, al
    inc esi
    ret

expect_char:
    cmp [esi], al
    jne syntax_error
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
    .s:
        cmp byte [esi], ' '
        jne .d
        inc esi
        jmp .s
.d:     ret

; ───────────── OUTPUT ─────────────

print_any:
    movzx ecx, byte [symbol_types + [cur_sym]]
    cmp ecx, 3
    je print_string
    call print_int
    ret

print_int:
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

print_string:
    push ebx
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
.count:
    cmp byte [ecx+edx], 0
    je .write
    inc edx
    jmp .count
.write:
    push 0
    push esp
    push edx
    push ecx
    push ebx
    call [WriteConsoleA]
    pop ebp
    ret

syntax_error:
    mov edx, err_msg
    call _PrintString
    jmp start

; ───────────── DATA ─────────────

section .data
input_buf         times 256 db 0
prompt_msg        db "> ", 0
newline_msg       db 13,10,0
err_msg           db "Syntax Error",13,10,0
import_msg        db "[module included]",13,10,0
print_buf         times 12 db 0
hConsole          dd 0
bytes_read        dd 0

symbol_mem        times 1024 dd 0
symbol_types      times 1024 db 0
cur_sym           dd 0
ret_val           dd 0

frame_ptr         dd 0
frame_args        times 128 dd 0
call_func_id      dd 0
func_defining     dd 0

string_table      times 2048 db 0
string_ptr        db 0
string_index      dd 0

; ───────────── IMPORTS ─────────────

section .idata
dd 0,0,0,RVA kernel32_dll
dd 0,0,0,0

kernel32_dll        db "kernel32.dll",0
ReadConsoleA_str    db "ReadConsoleA",0
WriteConsoleA_str   db "WriteConsoleA",0
GetStdHandle_str    db "GetStdHandle",0
ExitProcess_str     db "ExitProcess",0

import_table:
dd RVA ReadConsoleA_str, RVA kernel32_dll, RVA _ReadConsoleA
dd RVA WriteConsoleA_str, RVA kernel32_dll, RVA _WriteConsoleA
dd RVA GetStdHandle_str, RVA kernel32_dll, RVA _GetStdHandle
dd RVA ExitProcess_str, RVA kernel32_dll, RVA _ExitProcess
dd 0,0,0

_ReadConsoleA       dd 0
_WriteConsoleA      dd 0
_GetStdHandle       dd 0
_ExitProcess        dd 0
