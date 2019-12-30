@echo off
:A
echo Building tetrice.asm
tools\spasm -E -T tetrice.asm bin\CETRIS.8xp
echo Building tetrice_dat.asm
tools\spasm -E -T tetrice_dat.asm bin\CETRISDT.8xp
tools\windows_convbin -j 8x -k 8xv -i bin\CETRISDT.8xp -o bin\CETrisDT.8xv -n CETrisDT
Pause
Goto A