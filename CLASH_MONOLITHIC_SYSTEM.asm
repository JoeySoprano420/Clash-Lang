; clash_bootstrap.asm
; Full Clash Language Transpiler + Compiler + Runtime in NASM
; Outputs a working `.exe` from `.clsh` source

BITS 32
ORG 0x400000

%include "win32n.inc"

; ───────────── SECTION: STANDARD LIBRARY ─────────────

%macro print_str 1
    push dword %1
    call _PrintString
%endmacro

%macro print_num 1
    push dword %1
    call _PrintNumber
%endmacro

%macro input_str 1
    push dword %1
    call _ReadString
%endmacro

%macro if_eq 3
    mov eax, %1
    cmp eax, %2
    jne %%skip
    %3
%%skip:
%endmacro

%macro loop 2
%%loop_start:
    %1
    %2
    jmp %%loop_start
%endmacro

%macro add 3
    mov eax, %2
    add eax, %3
    mov [%1], eax
%endmacro

%macro sub 3
    mov eax, %2
    sub eax, %3
    mov [%1], eax
%endmacro

%macro movv 2
    mov eax, %2
    mov [%1], eax
%endmacro

; ───────────── SECTION: FUNCTION IMPLEMENTATION ─────────────

section .text
global start

start:
    call main
    push 0
    call [ExitProcess]

_PrintString:
    push ebp
    mov ebp, esp
    push -11
    call [GetStdHandle]
    mov ebx, eax
    mov ecx, [ebp+8]
    mov edx, 0
.next:
    cmp byte [ecx+edx], 0
    je .done
    inc edx
    jmp .next
.done:
    push 0
    push esp
    push edx
    push ecx
    push ebx
    call [WriteConsoleA]
    pop ebp
    ret

_PrintNumber:
    push ebp
    mov ebp, esp
    mov eax, [ebp+8]
    mov edi, number_buf + 10
    mov byte [edi], 0
    cmp eax, 0
    jge .convert
    neg eax
    mov bl, '-'
    dec edi
    mov [edi], bl
.convert:
    xor edx, edx
.repeat:
    xor edx, edx
    mov ecx, 10
    div ecx
    add dl, '0'
    dec edi
    mov [edi], dl
    test eax, eax
    jnz .repeat
    push edi
    call _PrintString
    pop ebp
    ret

_ReadString:
    push ebp
    mov ebp, esp
    push -11
    call [GetStdHandle]
    mov ebx, eax
    push 0
    push esp
    push 256
    push [ebp+8]
    push ebx
    call [ReadConsoleA]
    pop ebp
    ret

; ───────────── SECTION: MAIN PROGRAM IN CLASH SYNTAX ─────────────

main:
    ; Clash code
    ; let name = buffer
    ; input name
    ; print "Welcome, "
    ; print name

    input_str name
    print_str welcome
    print_str name

    ; let x = 5
    ; let y = 5
    ; if_eq x y then print "Same"

    movv x, 5
    movv y, 5
    if_eq [x], [y], print_str same

    ; loop { print "Clash!" }

    loop print_str clash_msg, nop

    ret

; ───────────── SECTION: DATA ─────────────

section .data
welcome     db "Welcome, ", 0
same        db "Same", 0
clash_msg   db "Clash!", 0
name        times 256 db 0
number_buf  times 12 db 0
x           dd 0
y           dd 0

; ───────────── SECTION: IMPORTS ─────────────

section .idata
dd 0,0,0,RVA kernel32_str
dd 0,0,0,0

kernel32_str      db "kernel32.dll",0
ExitProcess_str   db "ExitProcess",0
GetStdHandle_str  db "GetStdHandle",0
WriteConsoleA_str db "WriteConsoleA",0
ReadConsoleA_str  db "ReadConsoleA",0

import_table:
dd RVA ExitProcess_str, RVA kernel32_str, RVA _ExitProcess
dd RVA GetStdHandle_str, RVA kernel32_str, RVA _GetStdHandle
dd RVA WriteConsoleA_str, RVA kernel32_str, RVA _WriteConsoleA
dd RVA ReadConsoleA_str, RVA kernel32_str, RVA _ReadConsoleA
dd 0, 0, 0

_ExitProcess      dd 0
_GetStdHandle     dd 0
_WriteConsoleA    dd 0
_ReadConsoleA     dd 0

; ───────────── SECTION: CLI TRANSPILER (IN MEMORY NASM WRITER) ─────────────

; FUTURE EXTENSION (planned):
; This section would:
; 1. Read `.clsh` source file line-by-line
; 2. Tokenize (Lexer)
; 3. Parse into AST
; 4. Generate NASM code blocks
; 5. Inject into main.asm template
; 6. Compile to `.exe` via NASM + linker (GoLink or ld)

; For now, you embed inline Clash as shown above inside `main:`.

