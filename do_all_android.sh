adb devices
adb uninstall localhost.home.eugene.decoherence1
bash purge.sh
rm *.apk
castle-engine compile
castle-engine package --os=android --cpu=arm
castle-engine install --os=android --cpu=arm
castle-engine run --os=android --cpu=arm
