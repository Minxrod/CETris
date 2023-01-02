# CETris

It's Tetris, for the TI-84 Plus CE. Created entirely in ez80 assembly.

# Features for v0.8
* Both old rotations and SRS system
* Hold piece, next queue, and ghost piece
* Modifiable controls, button repeat, and game timings like lock delay/DAS
* Several options for block and background themes
* Several modes, including Marathon, Sprint, Dig, Cascade, Ultra, and more.
* Very customizable data format (better documentation pending)

# Future plans
* Remaining scoring mechanics (Combo, All-Clear detection)
* Dig challenge speed-up with time
* Better documentation of data file and code

# Controls
These can be changed from within the Options/Controls menu.
## Menu controls
* 2nd: Confirm
* Alpha: Back
* Left/Right: Select number (must select with 2nd/Confirm first)
* Up/Down: Change item selected
* Del: quit (works anywhere)

## Game controls
* 2nd/Alpha - rotate left/right
* Left/Right - move
* Down - soft drop
* Up - hard drop (instant drop + lock)
* XT0N - hold piece
* Mode - pause

# Known Issues
* Some specific action with unknown timing seems to cause the last move
left or right to be undone?

# Building 
You will need convbin and spasm-ng (see Tools section) placed in the tools/ directory. If you are on Windows and want to build, try using MinGW, the build script only works on Linux or similar.

Run `./build.sh` to build both the data file and the program file, as well as the test file. By default, this does not build with the Cesium header. Just add any argument, for example `./build.sh 1` to build with the Cesium header (this is the version with an icon)

# Useful Resources 
## Tools
* ez80 Notepad++ highlighting from https://www.cemetech.net/forum/viewtopic.php?p=243171#243171)
* spasm-ng (https://github.com/alberthdev/spasm-ng)
* convbin (https://github.com/mateoconlechuga/convbin)
* CEmu (https://github.com/CE-Programming/CEmu)
* CEtris Data Converter (https://github.com/Minxrod/CETris-Data-Converter)
## References
* Cemetech - https://cemetech.net
* NullPoMino - https://github.com/nullpomino/nullpomino
* Tetric A - https://www.cemetech.net/downloads/files/1347/x1347
* tetris.wiki
* wikiti.brandonw.net
