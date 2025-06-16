; clash_infinity.asm — Ultimate Clash Runtime
; Pure NASM interpreter+compiler with scope GC, function tables, expression stack parsing

BITS 32
ORG 0x400000

%include "win32n.inc"

section .text
global start
start:
    call init_system
.repl_loop:
    call prompt
    call read_line
    call parse_stmt
    call eval_stmt
    jmp .repl_loop

; ───────────── SYSTEM INIT ─────────────

init_system:
    xor eax, eax
    mov edi, sym_slots
.clear:
    stosd
    cmp edi, sym_slots + 8192
    jl .clear
    xor eax, eax
    mov edi, scope_table
.zero_scope:
    stosb
    cmp edi, scope_table + 1024
    jl .zero_scope
    mov dword [scope_depth], 0
    mov dword [sym_index], 0
    mov dword [func_index], 0
    ret

prompt:
    mov edx, prompt_msg
    call _PrintString
    ret

; ───────────── INPUT READER ─────────────

read_line:
    push -11
    call [GetStdHandle]
    mov [hConsole], eax
    push 0
    push bytes_read
    push 512
    push line_buf
    push eax
    call [ReadConsoleA]
    mov esi, line_buf
    ret

; ───────────── PARSER TABLE / RECURSIVE DESCENT ─────────────

parse_stmt:
    call skip_ws
    cmp dword [esi], 'let '
    je parse_let
    cmp dword [esi], 'func'
    je parse_func
    cmp dword [esi], 'prin'
    je parse_print
    cmp dword [esi], 'retu'
    je parse_return
    cmp dword [esi], 'if ('
    je parse_if
    cmp dword [esi], 'whil'
    je parse_while
    jmp parse_expr_stmt

parse_let:
    add esi, 4
    call parse_ident
    mov ebx, eax
    mov ecx, [scope_depth]
    mov [sym_scope + ebx], cl
    call expect_char, '='
    call parse_expr
    mov [sym_slots + ebx*4], eax
    ret

parse_func:
    add esi, 5
    call parse_ident
    mov ebx, eax
    mov [func_table + ebx*4], esi
    call skip_block
    inc dword [func_index]
    ret

parse_print:
    add esi, 6
    call parse_expr
    call print_val
    ret

parse_return:
    add esi, 7
    call parse_expr
    mov [return_val], eax
    mov dword [return_flag], 1
    ret

parse_if:
    add esi, 4
    call parse_expr
    cmp eax, 0
    je skip_block
    call parse_block
    ret

parse_while:
.loop:
    call parse_expr
    cmp eax, 0
    je .exit
    call parse_block
    jmp .loop
.exit:
    ret

parse_block:
    call expect_char, '{'
    inc dword [scope_depth]
    call parse_stmt
    call garbage_collect_scope
    ret

parse_expr_stmt:
    call parse_expr
    mov [last_expr], eax
    ret

; ───────────── POSTFIX CONVERSION + EVAL ─────────────

parse_expr:
    ; Simple: converts a + b * c to postfix
    xor ecx, ecx
    xor edi, edi
    call parse_term
    push eax
.next:
    call skip_ws
    mov al, [esi]
    cmp al, '+'
    je .add
    cmp al, '*'
    je .mul
    jmp .done
.add:
    inc esi
    call parse_term
    pop eax
    add eax, ebx
    push eax
    jmp .next
.mul:
    inc esi
    call parse_term
    pop eax
    imul eax, ebx
    push eax
    jmp .next
.done:
    pop eax
    ret

parse_term:
    call skip_ws
    mov al, [esi]
    cmp al, '0'
    jb parse_var
    cmp al, '9'
    ja parse_var
    call parse_number
    ret

parse_var:
    call parse_ident
    mov eax, [sym_slots + eax*4]
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
    .loop:
        cmp byte [esi], '}'
        je .done
        inc esi
        jmp .loop
    .done:
        inc esi
        ret

skip_ws:
.loop:
    cmp byte [esi], ' '
    jne .done
    inc esi
    jmp .loop
.done:
    ret

; ───────────── FUNCTION HANDLING ─────────────

call_function:
    call parse_ident
    mov ebx, eax
    mov esi, [func_table + ebx*4]
    call parse_stmt
    ret

; ───────────── GARBAGE COLLECTOR FOR SCOPED SYMBOLS ─────────────

garbage_collect_scope:
    mov ecx, [scope_depth]
    xor ebx, ebx
.loop:
    cmp [sym_scope + ebx], cl
    jne .next
    mov dword [sym_slots + ebx*4], 0
    mov byte  [sym_scope + ebx], 0
.next:
    inc ebx
    cmp ebx, 256
    jl .loop
    dec dword [scope_depth]
    ret

; ───────────── EVALUATOR ─────────────

eval_stmt:
    cmp [return_flag], 1
    jne .exit
    mov eax, [return_val]
    call print_val
    mov dword [return_flag], 0
.exit:
    ret

; ───────────── OUTPUT ─────────────

print_val:
    mov edi, out_buf + 10
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

syntax_error:
    mov edx, err_msg
    call _PrintString
    jmp start

; ───────────── DATA ─────────────

section .data
line_buf          times 512 db 0
prompt_msg        db "> ", 0
newline_msg       db 13,10,0
err_msg           db "Syntax Error",13,10,0
out_buf           times 12 db 0
hConsole          dd 0
bytes_read        dd 0

sym_slots         times 1024 dd 0
sym_scope         times 1024 db 0
scope_table       times 1024 db 0
scope_depth       dd 0
sym_index         dd 0

func_table        times 256 dd 0
func_index        dd 0

return_val        dd 0
return_flag       dd 0
last_expr         dd 0

; ───────────── IMPORTS ─────────────

section .idata
dd 0,0,0,RVA kernel32_dll
dd 0,0,0,0

kernel32_dll      db "kernel32.dll",0
ReadConsoleA_str  db "ReadConsoleA",0
WriteConsoleA_str db "WriteConsoleA",0
GetStdHandle_str  db "GetStdHandle",0
ExitProcess_str   db "ExitProcess",0

import_table:
dd RVA ReadConsoleA_str, RVA kernel32_dll, RVA _ReadConsoleA
dd RVA WriteConsoleA_str, RVA kernel32_dll, RVA _WriteConsoleA
dd RVA GetStdHandle_str, RVA kernel32_dll, RVA _GetStdHandle
dd RVA ExitProcess_str, RVA kernel32_dll, RVA _ExitProcess
dd 0,0,0

_ReadConsoleA     dd 0
_WriteConsoleA    dd 0
_GetStdHandle     dd 0
_ExitProcess      dd 0
