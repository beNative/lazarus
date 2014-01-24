/usr/bin/strip $1
#/Applications/iUPX.app/Contents/Resources/upx $1
rm ./bin/i386-darwin-carbon/notepas.app/Contents/MacOS/notepas
mv $1 ./bin/i386-darwin-carbon/notepas.app/Contents/MacOS/ 
