; Bootloader for Clashup OS
[org 0x7c00]
start:
    mov si, msg
    call print
    jmp $

print:
    mov ah, 0x0E
.next:
    lodsb
    or al, al
    jz .done
    int 0x10
    jmp .next
.done:
    ret

msg db "ClashupOS Bootloader - Hello Clash!", 0

times 510-($-$$) db 0
dw 0xAA55
