mkdir Release
copy "*.dll" "Release\*.dll"
copy "LICENSE" "Release\LICENSE"
copy "readme.txt" "Release\readme.txt"
copy "readme_ru.txt" "Release\readme_ru.txt"
copy "Decoherence.exe" "Release\decoherence.exe"
xcopy /s "data" "Release\data\"
