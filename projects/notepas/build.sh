# simple buildscript to make release builds for all supported platforms

typhonbuild --bm="Release win64 win32" notepas.lpr -B
typhonbuild --bm="Release win32 win32" notepas.lpr -B
typhonbuild --bm="Release win32 qt" notepas.lpr -B
typhonbuild --bm="Release win32 gtk2" notepas.lpr -B
typhonbuild --bm="Release linux32 gtk2" notepas.lpr -B
typhonbuild --bm="Release linux64 gtk2" notepas.lpr -B
typhonbuild --bm="Release linux64 qt" notepas.lpr -B

