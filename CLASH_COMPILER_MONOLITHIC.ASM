; clash_compiler_monolithic.asm
; Self-contained NASM-only Clash Language Transpiler + Compiler + Executor

BITS 32
ORG 0x400000

%include "win32n.inc"

section .text
global start
start:
    call init_memory_pool
    call transpile_clsh_file
    call invoke_nasm_compiler
    call invoke_golink_linker
    push 0
    call [ExitProcess]

; ───────────── INIT VARIABLE MEMORY POOL ─────────────

init_memory_pool:
    mov edi, var_pool
    xor eax, eax
.clear:
    stosd
    cmp edi, var_pool + 4096
    jl .clear
    ret

; ───────────── READ & TRANSLATE .CLSH SOURCE FILE ─────────────

transpile_clsh_file:
    push input_mode
    push input_path
    call [CreateFileA]
    mov [hFileIn], eax

    push output_mode
    push output_path
    call [CreateFileA]
    mov [hFileOut], eax

.read_next:
    mov esi, buffer
    push bytes_read
    push 256
    push buffer
    push [hFileIn]
    call [ReadFile]
    test eax, eax
    je .done

    call tokenize_line
    call translate_tokens
    call write_asm_line
    jmp .read_next

.done:
    push [hFileIn]
    call [CloseHandle]
    push [hFileOut]
    call [CloseHandle]
    ret

; ───────────── LEXICAL TOKENIZER ─────────────

tokenize_line:
    mov edi, token_buf
    xor ecx, ecx
    mov al, [esi]
.next_char:
    cmp al, 0
    je .end
    cmp al, ' '
    je .skip
    cmp al, 10
    je .skip
    mov [edi], al
    inc edi
.skip:
    inc esi
    mov al, [esi]
    jmp .next_char
.end:
    mov byte [edi], 0
    ret

; ───────────── TRANSLATE TOKENS TO ASM ─────────────

translate_tokens:
    mov esi, token_buf
    mov edi, asm_line

    ; Handle: let x = 5
    cmp dword [esi], 'let '
    jne .check_print
    call parse_let
    ret

.check_print:
    cmp dword [esi], 'prin'
    jne .check_input
    call parse_print
    ret

.check_input:
    cmp dword [esi], 'inpu'
    jne .check_if
    call parse_input
    ret

.check_if:
    cmp word [esi], 'if'
    jne .check_loop
    call parse_if_eq
    ret

.check_loop:
    cmp dword [esi], 'loop'
    jne .check_label
    call parse_loop
    ret

.check_label:
    cmp byte [esi], ':'
    jne .default
    call parse_label
    ret

.default:
    call copy_comment
    ret

; ───────────── PARSE HANDLERS ─────────────

parse_let:
    ; Expects: let var = const
    ; Generates: mov dword [varname], value
    mov ecx, let_template
    call copy_string
    call extract_var_name
    call extract_const_value
    ret

parse_print:
    ; print "Hello"
    mov ecx, print_template
    call copy_string
    call encode_string_literal
    ret

parse_input:
    mov ecx, input_template
    call copy_string
    ret

parse_if_eq:
    ; if_eq x 5 then labelX
    mov ecx, if_template
    call copy_string
    call extract_condition
    ret

parse_loop:
    mov ecx, loop_template
    call copy_string
    ret

parse_label:
    ; :start
    mov ecx, label_template
    call copy_string
    ret

copy_comment:
    mov ecx, comment_template
    call copy_string
    ret

; ───────────── TOKEN UTILITIES ─────────────

extract_var_name:
    ; get variable after "let "
    mov ecx, token_buf + 4
    mov edi, asm_line + 15
    call copy_until_space
    ret

extract_const_value:
    ; get number after '='
    mov ecx, token_buf
.seek:
    cmp byte [ecx], '='
    je .found
    inc ecx
    jmp .seek
.found:
    inc ecx
    mov edi, asm_line + 25
    call copy_until_space
    ret

extract_condition:
    ; rudimentary: locate numbers after if_eq
    mov ecx, token_buf + 6
    mov edi, asm_line + 15
    call copy_until_space
    inc ecx
    mov edi, asm_line + 22
    call copy_until_space
    ret

encode_string_literal:
    ; Add string to memory pool
    mov ecx, token_buf + 6
    mov edi, mem_strings
    mov esi, ecx
.strcopy:
    lodsb
    test al, al
    je .done
    stosb
    jmp .strcopy
.done:
    ret

copy_until_space:
    lodsb
    cmp al, 0
    je .fin
    cmp al, ' '
    je .fin
    stosb
    jmp copy_until_space
.fin:
    mov byte [edi], 0
    ret

; ───────────── WRITE GENERATED ASM LINE TO FILE ─────────────

write_asm_line:
    push bytes_written
    push asm_line_len
    push asm_line
    push [hFileOut]
    call [WriteFile]
    ret

; ───────────── BUILD SYSTEM: COMPILE AND LINK ─────────────

invoke_nasm_compiler:
    push 0
    push 0
    push 0
    push nasm_cmd
    call [WinExec]
    ret

invoke_golink_linker:
    push 0
    push 0
    push 0
    push golink_cmd
    call [WinExec]
    ret

; ───────────── DATA SECTIONS ─────────────

section .data
input_path     db "input.clsh", 0
output_path    db "output.asm", 0
input_mode     dd 0x80000000
output_mode    dd 0x40000000
hFileIn        dd 0
hFileOut       dd 0
bytes_read     dd 0
bytes_written  dd 0

buffer         times 256 db 0
token_buf      times 256 db 0
asm_line       times 512 db 0
asm_line_len   equ 512

mem_strings    times 1024 db 0
var_pool       times 4096 db 0

print_template db "print_str ", 0
input_template db "input_str ", 0
let_template   db "mov dword [",0
if_template    db "cmp eax, ebx", 0
loop_template  db "loop ", 0
label_template db "", 0
comment_template db "; unknown or comment", 0

nasm_cmd       db "cmd /c nasm -f win32 output.asm -o output.obj", 0
golink_cmd     db "cmd /c GoLink output.obj kernel32.dll", 0

; ───────────── IMPORT SECTION ─────────────

section .idata
dd 0,0,0,RVA kernel32_str
dd 0,0,0,0

kernel32_str        db "kernel32.dll",0
CreateFileA_str     db "CreateFileA",0
ReadFile_str        db "ReadFile",0
WriteFile_str       db "WriteFile",0
CloseHandle_str     db "CloseHandle",0
WinExec_str         db "WinExec",0
ExitProcess_str     db "ExitProcess",0

import_table:
dd RVA CreateFileA_str, RVA kernel32_str, RVA _CreateFileA
dd RVA ReadFile_str, RVA kernel32_str, RVA _ReadFile
dd RVA WriteFile_str, RVA kernel32_str, RVA _WriteFile
dd RVA CloseHandle_str, RVA kernel32_str, RVA _CloseHandle
dd RVA WinExec_str, RVA kernel32_str, RVA _WinExec
dd RVA ExitProcess_str, RVA kernel32_str, RVA _ExitProcess
dd 0,0,0

_CreateFileA        dd 0
_ReadFile           dd 0
_WriteFile          dd 0
_CloseHandle        dd 0
_WinExec            dd 0
_ExitProcess        dd 0
