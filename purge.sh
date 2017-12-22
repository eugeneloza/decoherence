# Remove compiler garbage, backups and etc.

rm *.o
rm *.ppu
rm *.so
rm *.dbg
rm *.bak

rm -r backup
rm -r lib
rm -r src/backup
rm -r src/lib

# Remove ugly Linux-related bug with NTFS filesystem

rm .fuse_hidden*

# Remove logs

rm *.log
rm heap.trc
