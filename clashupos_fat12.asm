; Bootable Clashup OS Loader: FAT12 + .clsh file mini-loader
; Assembles with: nasm -f bin clashupos_fat12.asm -o clashupos.img

org 0x7c00

start:
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7c00

    mov si, bootmsg
    call print

    ; Load root directory (FAT12) - at 19th sector (0x13), 14 sectors (224 entries)
    mov ax, 0x020e  ; AH=02 (read), AL=0x0e sectors (root), drive 0
    mov cx, 0x0014  ; CH=0, CL=20 (start sector: 19+1), so CL=0x14
    mov dx, 0x0000  ; DH=0, DL=0 (floppy)
    mov bx, 0x0500  ; ES:BX=0:0500 - load root dir
    int 0x13
    jc disk_error

    ; Search for file "SCRIPT  CLS"
    mov si, 0x0500
find_entry:
    mov di, filename
    mov cx, 11
    push si
    repe cmpsb
    pop si
    je found
    add si, 32
    cmp si, 0x0500+224*32
    jl find_entry
    jmp not_found

found:
    ; Get first cluster number (offset 26 in entry)
    add si, 26
    mov ax, [si]
    ; Load cluster (FAT12, data area starts at sector 33)
    sub ax, 2
    mov bx, 512
    mul bx
    add ax, 33*512
    mov bx, 0x0600
    mov es, ax
    mov ax, 0x0201
    mov cx, 0x0021 ; sector 33+1=34, 1 sector
    mov dx, 0x0000
    int 0x13
    jc disk_error

    mov si, 0x0600
    call interpret_clsh
    jmp $

disk_error:
    mov si, diskmsg
    call print
    jmp $

not_found:
    mov si, notfound
    call print
    jmp $

print:
    lodsb
    or al, al
    jz .done
    mov ah, 0x0e
    int 0x10
    jmp print
.done:
    ret

bootmsg db "ClashupOS Boot: FAT12 Loader", 13, 10, 0
diskmsg db "Disk read error!", 13, 10, 0
notfound db "SCRIPT.CLS not found", 13, 10, 0
filename db 'SCRIPT  CLS'

; Mini interpreter: print lines starting with '!'
interpret_clsh:
    lodsb
    cmp al, '!'
    jne .next
    call print_line
.next:
    cmp al, 0
    jne interpret_clsh
    ret

print_line:
    lodsb
    or al, al
    jz .done
    mov ah, 0x0e
    int 0x10
    cmp al, 13
    jne print_line
.done:
    ret

times 510-($-$$) db 0
dw 0xAA55
