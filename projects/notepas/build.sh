# simple buildscript to make release builds for all supported platforms
rm lib -r
typhonbuild --bm="Release win64 x86_64 win32" notepas.lpr -B
typhonbuild --bm="Release win32 i386 win32" notepas.lpr -B
typhonbuild --bm="Release win32 i386 qt" notepas.lpr -B
typhonbuild --bm="Release win32 i386 gtk2" notepas.lpr -B
typhonbuild --bm="Release linux i386 gtk2" notepas.lpr -B
typhonbuild --bm="Release linux x86_64 gtk2" notepas.lpr -B
typhonbuild --bm="Release linux x86_64 qt" notepas.lpr -B

