# Do a basic clean-up

bash purge.sh

# Remove compiled files and screenshots

rm pasdoc/html/*
rm -r Release
rm *.apk
rm *.jpg
rm *.scr.png

# Compiled executables

rm Decoherence_Desktop
rm Decoherence_Desktop.exe
rm Constructor_Tool
rm Constructor_Tool.exe
rm tests/profiler/ProfilerTest
