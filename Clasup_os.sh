mkfs.vfat -F 12 clashupos.img
mount -o loop clashupos.img /mnt
cp SCRIPT.CLS /mnt
umount /mnt
