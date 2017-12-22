# Remove compiler garbage, backups and etc.

rm *.o
rm *.ppu
rm *.so
rm *.log
rm *.dbg
rm *.bak
rm -r android_application/

rm -r backup
rm -r lib
rm -r src/backup
rm -r src/lib
rm -r tests/profiler/backup
rm -r tests/profiler/lib

# Remove ugly Linux-related bug with NTFS filesystem

rm .fuse_hidden*

# Remove logs

rm log_*.txt
rm heap.trc
