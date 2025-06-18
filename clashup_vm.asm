; Pure Assembly Clashup VM — supports:
; let, print, input (minimal bytecode)
; Example .clsh bytecode: [01][var][value] [02][var] [03][var]

org 0x7c00

start:
    mov si, script
.next:
    lodsb
    cmp al, 1  ; let
    je .let
    cmp al, 2  ; print
    je .print
    cmp al, 3  ; input (not implemented)
    je .input
    cmp al, 0
    je .halt
    jmp .next

.let:
    lodsb   ; var index
    mov bl, al
    lodsb   ; value
    mov [vars+bx], al
    jmp .next

.print:
    lodsb
    mov bl, al
    mov al, [vars+bx]
    add al, '0'
    mov ah, 0x0e
    int 0x10
    jmp .next

.input:
    ; Not implemented
    jmp .next

.halt:
    jmp $

vars: times 8 db 0

; Example script: let x=5; print x
script: db 1, 0, 5, 2, 0, 0

times 510-($-$$) db 0
dw 0xAA55
