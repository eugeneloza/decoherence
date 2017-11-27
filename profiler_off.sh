cd src
sed -i -- 's/StartProfiler;/{StartProfiler}/g' *.pas
sed -i -- 's/StartProfiler;/{StartProfiler}/g' *.inc
sed -i -- 's/StopProfiler;/{StopProfiler}/g' *.pas
sed -i -- 's/StopProfiler;/{StopProfiler}/g' *.inc
cd ..