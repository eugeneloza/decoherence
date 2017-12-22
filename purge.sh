# Remove compiler garbage, backups and etc.

rm *.dbg
find -type d -name 'backup' -prune -exec rm -rf {} \;
find -type d -name 'lib' -prune -exec rm -rf {} \;

# Remove ugly Linux-related bug with NTFS filesystem

find . -name '.fuse_hidden*' -delete

# Remove logs

rm *.log
rm heap.trc
