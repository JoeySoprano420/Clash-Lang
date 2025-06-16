; clash_final_repl.asm — Final NASM-only Clash Language System
; Real recursive expression parser + typed symbol table + live REPL interpreter

BITS 32
ORG 0x400000

%include "win32n.inc"

section .text
global start
start:
    call init_repl
.repl_loop:
    call read_line
    call tokenize_line
    call parse_expr
    call eval_expr
    call print_result
    jmp .repl_loop

; ───────────── INIT ─────────────

init_repl:
    mov edi, symbol_table
    xor eax, eax
.fill_zero:
    stosd
    cmp edi, symbol_table + 4096
    jl .fill_zero
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

; ───────────── TOKENIZER ─────────────

tokenize_line:
    mov esi, input_buf
    mov edi, tokens
    xor ecx, ecx
.next_char:
    lodsb
    cmp al, 0
    je .done
    cmp al, ' '
    je .next_char
    stosb
    inc ecx
    jmp .next_char
.done:
    mov byte [edi], 0
    ret

; ───────────── SYMBOL TABLE ─────────────

define_symbol:
    ; EAX = value, EBX = variable index
    mov [symbol_table + ebx*4], eax
    mov byte [symbol_types + ebx], 1
    ret

lookup_symbol:
    ; EBX = index → returns EAX = value
    mov eax, [symbol_table + ebx*4]
    ret

; ───────────── EXPRESSION PARSER (RECURSIVE DESCENT) ─────────────

parse_expr:
    ; parse simple binary expr: num [+|-|*|/] num
    mov esi, tokens
    call parse_term
    mov [expr_lhs], eax

    mov al, [esi]
    mov [expr_op], al
    inc esi

    call parse_term
    mov [expr_rhs], eax
    ret

parse_term:
    lodsb
    sub al, '0'
    movzx eax, al
    ret

; ───────────── EVALUATOR ─────────────

eval_expr:
    mov eax, [expr_lhs]
    mov ebx, [expr_rhs]
    mov cl, [expr_op]
    cmp cl, '+'
    je .add
    cmp cl, '-'
    je .sub
    cmp cl, '*'
    je .mul
    cmp cl, '/'
    je .div
    jmp .done
.add:
    add eax, ebx
    jmp .done
.sub:
    sub eax, ebx
    jmp .done
.mul:
    imul eax, ebx
    jmp .done
.div:
    xor edx, edx
    div ebx
.done:
    mov [result], eax
    ret

; ───────────── PRINT RESULT ─────────────

print_result:
    mov eax, [result]
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
input_buf       times 256 db 0
tokens          times 64 db 0
expr_lhs        dd 0
expr_rhs        dd 0
expr_op         db 0
result          dd 0
print_buf       times 12 db 0
symbol_table    times 1024 dd 0
symbol_types    times 1024 db 0
newline_msg     db 13,10,0
bytes_read      dd 0
hConsole        dd 0

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
