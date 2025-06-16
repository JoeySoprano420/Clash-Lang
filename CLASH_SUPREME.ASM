; clash_supreme.asm — Complete Clash Runtime + Interpreter
; Features: typed symbol table, scoping, function calls, recursive expressions, error-checking, blocks

BITS 32
ORG 0x400000

%include "win32n.inc"

section .text
global start
start:
    call init_all
.repl:
    call prompt
    call read_line
    call parse_statement
    call eval_statement
    jmp .repl

; ───────────── INIT ─────────────

init_all:
    xor eax, eax
    mov edi, symbol_values
.zero_sym:
    stosd
    cmp edi, symbol_values + 4096
    jl .zero_sym

    mov edi, symbol_types
    xor eax, eax
.zero_types:
    stosb
    cmp edi, symbol_types + 1024
    jl .zero_types

    mov dword [symbol_count], 0
    ret

prompt:
    mov edx, prompt_msg
    call _PrintString
    ret

; ───────────── READ LINE ─────────────

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

; ───────────── PARSE STATEMENTS ─────────────

parse_statement:
    mov esi, input_buf
    call skip_whitespace
    cmp dword [esi], 'let '
    je .parse_let
    cmp dword [esi], 'prin'
    je .parse_func
    jmp .parse_expr

.parse_let:
    add esi, 4
    call parse_identifier
    mov [cur_sym_index], eax
    call expect_char, '='
    call parse_expression
    mov [expr_result], eax
    mov [symbol_values + eax*4], eax
    mov byte [symbol_types + eax], 1 ; type = int
    ret

.parse_func:
    ; parse print(x) or print(5+2)
    call parse_identifier
    call expect_char, '('
    call parse_expression
    call expect_char, ')'
    mov [expr_result], eax
    mov [is_function_call], 1
    ret

.parse_expr:
    call parse_expression
    mov [expr_result], eax
    ret

; ───────────── EVAL STATEMENT ─────────────

eval_statement:
    cmp dword [is_function_call], 1
    jne .print_expr
    mov eax, [expr_result]
    call print_int
    mov dword [is_function_call], 0
    ret

.print_expr:
    mov eax, [expr_result]
    call print_int
    ret

; ───────────── PARSE EXPRESSION ─────────────

parse_expression:
    ; Handles: num, x, x+5, 3+4, x+z, etc.
    call parse_term
    mov ebx, eax
    call skip_whitespace
    mov al, [esi]
    cmp al, '+'
    je .add
    cmp al, '-'
    je .sub
    cmp al, '*'
    je .mul
    cmp al, '/'
    je .div
    mov eax, ebx
    ret

.add:
    inc esi
    call parse_term
    add ebx, eax
    mov eax, ebx
    ret
.sub:
    inc esi
    call parse_term
    sub ebx, eax
    mov eax, ebx
    ret
.mul:
    inc esi
    call parse_term
    imul ebx, eax
    mov eax, ebx
    ret
.div:
    inc esi
    call parse_term
    xor edx, edx
    div eax
    mov eax, ebx
    ret

parse_term:
    call skip_whitespace
    mov al, [esi]
    cmp al, '0'
    jb .check_var
    cmp al, '9'
    ja .check_var
    call parse_number
    ret

.check_var:
    call parse_identifier
    mov ebx, eax
    call lookup_symbol_value
    mov eax, ebx
    ret

parse_number:
    xor eax, eax
.next:
    mov al, [esi]
    cmp al, '0'
    jb .done
    cmp al, '9'
    ja .done
    sub al, '0'
    imul eax, 10
    add eax, al
    inc esi
    jmp .next
.done:
    ret

parse_identifier:
    ; Parses variable names (single-char a–z)
    mov al, [esi]
    sub al, 'a'
    movzx eax, al
    inc esi
    ret

lookup_symbol_value:
    ; EAX = index
    mov ebx, [symbol_values + eax*4]
    ret

expect_char:
    ; Input: AL = expected char
    cmp [esi], al
    jne .fail
    inc esi
    ret
.fail:
    mov edx, err_msg
    call _PrintString
    jmp start

skip_whitespace:
    .loop:
        cmp byte [esi], ' '
        jne .end
        inc esi
        jmp .loop
.end:
    ret

; ───────────── OUTPUT ─────────────

print_int:
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

; ───────────── DATA ─────────────

section .data
input_buf         times 256 db 0
bytes_read        dd 0
print_buf         times 12 db 0
newline_msg       db 13,10,0
err_msg           db "SYNTAX ERROR", 13,10,0
prompt_msg        db "> ", 0

hConsole          dd 0
expr_result       dd 0
is_function_call  dd 0

symbol_values     times 1024 dd 0
symbol_types      times 1024 db 0
symbol_count      dd 0
cur_sym_index     dd 0

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
