@echo off
:A
tools\spasm -E -T tetrice.asm bin\CETRIS.8xp
Pause
Goto A