nasm -f bin clashup_kernel.asm -o clashupos.img
qemu-system-x86_64 -fda clashupos.img
