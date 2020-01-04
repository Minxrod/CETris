#include "includes\ti84pce.inc"
#include "includes\tetrice.inc"
;#include "tetrice.asm"

 .assume ADL=1
 .org saveSScreen

dataReferences:
.dl spriteData ;std sprites like blocks, field, etc
.dl fontData ;font data. 
.dl blockGraphicData ;block sprite+palette pairs
.dl 0 ;what will this be?
.dl paletteData ;palette data
.dl menuObjData ;menu data
.dl pauseData
.dl SSSInfo ;ptr to item info for display in game
.dl gameOverData ;game over notif

;these references behave the same as data references
;but are for specific graphical elements,
;instead of being for data pointers.
;examples: references to...
;tetris field data
;hold data
;score/lines/level
;pretty much anything that requires the 
;occasional redraw as part of it's update code.
;make sure it points to SSS data,
;not the original data, because program reads from SSSInfo.
references:
.dl fieldInfo - itemsInfo + SSSInfo
.dl holdInfo - itemsInfo + SSSInfo
.dl levelInfo - itemsInfo + SSSInfo
.dl scoreInfo - itemsInfo + SSSInfo
.dl linesInfo - itemsInfo + SSSInfo
.dl previewInfo - itemsInfo + SSSInfo
.dl timerInfo - itemsInfo + SSSInfo

;note: address expected in tetrice.inc to be refSize * 16 + SSS. be sure to modify if refs are added.
initDat:
 call applyTheme
 ret

boxColor = 14
boxColor2= 15
textColor= 35
infoBoxX = 168
infoBoxY = 16

itemsInfo:
.db 10 ;number of items
fieldInfo:
.db typeTetris
.dw 0 ;x
.db 0 ;y
.db 12 ;a/size of tile
.dw field - PSS ;eventually, consider making the field memory movable
.db 10, 20 ;width, height (for display only)
holdInfo:
.db typeHold
.dw 132
.db 160
.db 12 ;size of tile
.dw 0 ;uses refSprite data ptr
.db 4, 4 ;width, height
previewInfo:
.db typePreview
.dw previewBox - SSS
.db 0 ;unused
.db 0 ;unused?
.dw previewCoords - SSS;SSS ptr to coords
.db 6, 0 ;# of preview blocks, unused
;BACKGROUND BOX INFO
.db typeBox
.dw infoBoxX ;x
.db infoBoxY ;y
.db boxColor ;color of main
.dw 128 ;width
.db boxColor2, 80 ;bordercolor, height
;SCORE TEXT ETC.
.db typeMenu
.dw infoBoxX + 8
.db infoBoxY + 8
.db textColor
.dw gameText - SSS
.db 7, 0 ;cursorid doesn't matter cause not a active menu
scoreInfo:
.db typeNumber
.dw infoBoxX + 8 + 48
.db infoBoxY + 8 + 0
.db textColor
.dw score - PSS ;variables are saved in PSS
.db 8, boxColor ; size of number, bgcolor
levelInfo:
.db typeNumber
.dw infoBoxX + 8 + 48
.db infoBoxY + 8 + 24
.db textColor
.dw level - PSS
.db 3, boxColor ;size number, bg color
linesInfo:
.db typeNumber
.dw infoBoxX + 8 + 48
.db infoBoxY + 8 + 32
.db textColor
.dw lines - PSS
.db 4, boxcolor ;number of digits, bgcolor
timerInfo:
.db typeNumber
.dw infoBoxX + 8 + 48
.db infoBoxY + 8 + 8
.db textColor
.dw globalTimer - PSS
.db 8, boxcolor
highscoreInfo:
.db typeNumber
.dw infoBoxX + 8 + 48
.db infoBoxY + 8 + 48
.db textColor
.dw highScore - PSS
.db 8, boxcolor

gameText
 .db "Score:",0
 .db "Timer:",0
 .db 0
 .db "Level:",0
 .db "Lines:",0
 .db 0
 .db " Best:",0
 
previewBox:
 .db typeBox
 .dw 126 ;x
 .db 6 ;y
 .db 2 ;color
 .dw 36 ;width
 .db 3, 144 ;border, height
 
previewCoords:
 .dw 132
 .db 12
 .dw 132
 .db 36
 .dw 132
 .db 60
 .dw 132
 .db 84
 .dw 132
 .db 108
 .dw 132
 .db 132
 
SSSInfo = itemsInfo

menuObjData:
 .db 4
 ;background
 .db typeBox
 .dw 0 ;x
 .db 0 ;y
 .db 5 ;color
 .dw 72 ;width
 .db 6, 40 ;bordercolor, height
 ;menu text
 .db typeMenu
 .dw 8
 .db 8
 .db textColor
 .dw menuText - SSS
 .db 3, 2 ;# items, cursorID within menuObjData
 ;cursor
 .db typeString
 .dw 0
 .db 8
 .db textColor
 .dw cursorString - SSS
 .db 0, 0
 ;logo
 .db typeSprite1bpp
 .dw 256
 .db 200
 .db 1
 .dw logoSprite - SSS
 .db 7, 14
 
cursorString:
 .db "*",0
 
menuText:
 .db "START",0
optionsText:
 .db "OPTIONS",0
 .db "EXIT",0
menuJumps:
 jp jptmainMenu ;back/return location.
 jp setupGame
 jp gotoOptions
 jp jptExit
 
setupGame:
 ld ix, startMenuData
 
 ld hl, modeText
 ld a,(rules+rMode) ;hm
 call jptGetStringInList
 ld (startMenuSelectMode + iDataPTRL),hl
 
 ld a,1
 ld (level),a ;default to 1 please
 jp jptActiveMenu
 
gotoOptions:
 ld ix, optionsMenuData
 jp jptActiveMenu
 
startMenuData:
 .db 6
  ;background
 .db typeBox
 .dw 0 ;x
 .db 0 ;y
 .db 2 ;color
 .dw 200 ;width
 .db 1, 48 ;bordercolor, height
 ;menu text
 .db typeMenu
 .dw 8
 .db 8
 .db textColor
 .dw startMenuText - SSS
 .db 4, 2 ;# items, cursorID within menuObjData
 ;cursor
 .db typeString
 .dw 0
 .db 8
 .db textColor
 .dw cursorString - SSS
 .db 0, 0
startMenuSelectMode:
 .db typeString
 .dw 48
 .db 8
 .db textColor
 .dw modeText - SSS
 .db 0, 2 ;bg color
startMenuSelectLev:
 .db typeNumber
 .dw 56
 .db 16
 .db textColor
 .dw level - PSS
 .db 2, 2 ;digits, bgcolor
startMenuSelectLD:
 .db typeNumber
 .dw 96
 .db 24
 .db textColor
 .dw lockDelay - PSS
 .db 2, 2 ;digits, bgcolor
 
startMenuText:
 .db "Mode:",0
 .db "Level:",0
 .db "Lock Delay:",0
 .db "BEGIN",0
startMenuJumps:
 jp jptMainMenu ;example of prev menu jump
 jp selectMode
 jp selectLev
 jp selectLockDelay
 jp jptInitGame ;starts game
 
selectLev:
 ld ix, startMenuSelectLev
 ld a, 21
 call jptSetNumber
 
 ;after setting number,
 ;return to main start menu.
 ld ix, startMenuData
 jp jptActiveMenu
 
selectLockDelay:
 ld ix, startMenuSelectLD
 ld a, 61
 call jptSetNumber
 
 ;after setting number,
 ;return to main start menu.
 ld ix, startMenuData
 jp jptActiveMenu

selectMode:
 ld ix, selectModeMenu
 jp jptActiveMenu
 
selectModeMenu:
 .db 3
 ;background
 .db typeBox
 .dw 0 ;x
 .db 0 ;y
 .db 9 ;color
 .dw 160 ;width
 .db 10, 160 ;bordercolor, height
 ;menu text
 .db typeMenu
 .dw 8
 .db 8
 .db textColor
 .dw modeText - SSS
 .db 18, 2 ;# items, cursorID within menuObjData
 ;cursor
 .db typeString
 .dw 0
 .db 8
 .db textColor
 .dw cursorString - SSS
 .db 0, 0

modeText:
.db "MARATHON-150",0
.db "MARATHON-200",0
.db "MARATHON-ENDLESS",0
.db "RETRO-150",0
.db "RETRO-200",0
.db "RETRO-ENDLESS",0
.db "SPRINT-20",0
.db "SPRINT-40",0
.db "DIG-5",0
.db "DIG-10",0
.db "DIG-15",0
.db "DIG CHALLENGE",0
.db "EXCAVATE-10-LIGHT",0
.db "EXCAVATE-10-MEDIUM",0
.db "EXCAVATE-10-DENSE",0
.db "CASCADE-150",0
.db "CASCADE-200",0
.db "CASCADE-ENDLESS",0
modeJumps:
 jp setupGame ;prev menu
 ;note: ld a,x/jr setLines = 4byte alignment
 ld a,150
 jr setMarathon
 ld a,200
 jr setMarathon
 ld a,0
 jr setMarathon
 ld a,150
 jr setRetro
 ld a,200
 jr setRetro
 ld a,0
 jr setRetro
 ld a,20
 jr setLineRace
 ld a,40
 jr setLineRace
 ld a,5
 jr setDig
 ld a,10
 jr setDig
 ld a,15
 jr setDig
 ld a,180
 jr setRising
 ld a,2 << 5 | 10
 jr setExcavate
 ld a,5 << 5 | 10
 jr setExcavate
 ld a,7 << 5 | 10
 jr setExcavate
 ld a,150
 jr setCascade
 ld a,200
 jr setCascade
 ld a,0
 jr setCascade
 
setMarathon:
 ld e,0
 call setModeFromE
 jr setLines
 
setRetro:
 ld e,1
 call setModeFromE
 jr setLines

setLineRace:
 ld e,2
 call setModeFromE
 jr setLines
 
setDig:
 ld e,3
 call setModeFromE
 jr setGeneration

setExcavate: 
 ld e,3
 call setModeFromE
 jr setGenGGD

setRising:
 ld e,4
 call setModeFromE
 jr setRisingInfo
 
setCascade:
 ld e,5
 call setModeFromE
 jr setLines
 
setLines:
 cp 0
 jr z, setNoWin
 ld (ix+rLCW),a
 
 ;return to setup menu
slToMenu:
 ;replaces selected mode string
 call jptGetStringPTRSelection
 ld (startMenuSelectMode + iDataPTRL),hl
 
 ld ix, rules
 ld a,(menuSelection)
 ld (ix+rMode),a ;set mode
 
 ld ix, startMenuData 
 jp jptActiveMenu
 
setNoWin:
 ld (ix+rfWin),a ;a=0
 jr slToMenu

setGeneration:
 ld (ix+rGGD),9 ;9 blocks per row
 ld (ix+rGGA),a
 jr slToMenu

setGenGGD:
 ld e,a
 and $1f
 ld (ix+rGGA),a ;rows
 xor a
 ld b,3
eback3:
 rl e
 rla
 djnz eback3
 inc a ;generate 1-8 density, not 0-7 because 0 is pointless anyways
 ld (ix+rGGD),a
 jr slToMenu
 
setRisingInfo:
 ld (ix+rRGT),a
 ld (ix+rRGD),9
 jr slToMenu
 
modeData:
 .db rMarathon, rNull, rLines, rScore ;marathon
 .db rRetro, rNull, rLines, rScore ;retro
 .db rMarathon, rNull, rLines, rTime ;line race
 .db rMarathon, rGenerated, rRow0, rTime ;dig/excavate
 .db rMarathon, rRising, rNull, rLine
 .db rMarathon, rCascadeExtra, rLines, rScore ;cascade
 
setModeFromE:
 ld b,e
 ld de,0
 push de
 ld e,b ;ensure de in 1 byte
 ld hl, modeData - SSS + SSS
 add hl,de
 add hl,de
 add hl,de
 add hl,de ;4x mode in e
 ld de,rules
 pop bc ;bc=0
 ld c,4
 ldir ;copy from modedata+mode*4 to rules
 ld ix,rules ;useful
 ret
 
optionsMenuData:
 .db 5
;background box
 .db typeBox
 .dw 0 ;x
 .db 0 ;y
 .db 18 ;color
 .dw 80 ;width
 .db 19, 40 ;bordercolor, height
;heading
 .db typeString
 .dw 0
 .db 8
 .db textColor
 .dw optionsText - SSS
 .db 0, 0
;menu
 .db typeMenu
 .dw 8
 .db 16
 .db textColor
 .dw optionsMenuText - SSS
 .db 2, 3
;curosr
 .db typeString
 .dw 0
 .db 16
 .db textColor
 .dw cursorString - SSS
 .db 0, 0
;theme
themeSelection:
 .db typeNumber
 .dw 56
 .db 16
 .db textColor
 .dw theme - PSS
 .db 3, 18
 
optionsMenuText:
 .db "THEME:",0
 .db "CONTROLS",0
optionJumps:
 jp jptMainMenu
 jp setTheme
 jp controlMenu
 
controlMenu:
 ld ix,controlMenuData
 jp jptActiveMenu
 
setTheme:
 ld ix,themeSelection
 ld a,4
 call jptSetNumber
 
 call applyTheme
 
 ld ix, optionsMenuData
 jp jptActiveMenu ;back to options menu
 
applyTheme:
 ld a,(theme) ;value just set here
 ld hl,(drefBlocks)
 ld b,7
replaceBlockSpriteID:
 ld (hl),a
 inc hl ;palette
 inc hl ;next block spriteid
 djnz replaceBlockSpriteID
 ;all spriteid has been replaced, back to option
 ret
 
controlMenuData:
 .db 5
;background
 .db typeBox
 .dw 0
 .db 0
 .db 22
 .dw 132
 .db 23, 40
;menu
 .db typeMenu
 .dw 8
 .db 8
 .db textColor
 .dw controlMenuText - SSS
 .db 3, 2
;cursor
 .db typeString
 .dw 0
 .db 8
 .db textColor
 .dw cursorString - SSS
 .db 0, 0
keyRepeatLR:
 .db typeNumber8
 .dw 104
 .db 8
 .db textColor
 .dw buttonLeft + buttonTimeRepeat - PSS
 .db 3, 22 ;digits, bgcolor
keyRepeatDrop:
 .db typeNumber8
 .dw 104
 .db 16
 .db textColor
 .dw buttonSoft + buttonTimeRepeat - PSS
 .db 3, 22 ;digits, bgcolor
 
controlMenuText:
 .db "MOVE REPEAT:",0
 .db "DROP REPEAT:",0
 .db "SET CONTROLS",0
controlJumps:
 jp gotoOptions
 jp setButtonInfoLR
 jp setButtonInfoDrop
 jp buttonMappingMenu
 
setButtonInfoLR:
 ld ix, keyRepeatLR
 ld a,255
 call jptSetNumber
 
 ;affect both LEFT and RIGHT.
 ld a,(numberSelection)
 ld hl,buttonRight+buttonTimeRepeat
 ld (hl),a
 
 jp controlMenu
 
setButtonInfoDrop:
 ld ix, keyRepeatDrop
 ld a,255
 call jptSetNumber

 jp controlMenu
 
buttonMappingMenu:
 ld ix, buttonMappingData
 jp jptActiveMenu
 
buttonMappingData:
 .db 4
;bg box
 .db typeBox
 .dw 0
 .db 0
 .db 25
 .dw 180
 .db 26, 96
;menu text
 .db typeMenu
 .dw 8
 .db 8
 .db textColor
 .dw remapMenuText - SSS
 .db 8, 2
;cursor
 .db typeString
 .dw 0
 .db 8
 .db textColor
 .dw cursorString - SSS
 .db 0, 0
;button info
 .db typeCustom
 .dw 0
 .db 0
 .db 0
 .dw listButtonMaps - SSS
 .db 0, 8

buttonPrompt:
 .db typeString
 .dw 8
 .db 80
 .db textColor
 .dw buttonPromptText - SSS
 .db 0, 0
buttonPromptText:
 .db "PRESS BUTTON TO SET",0
 
tempText:
 .db typeString
 .dw 128
 .db 0
 .db textColor
 .dw 0
 .db 0, 0
 
remapMenuText:
 .db "LEFT MOVE",0
 .db "RIGHT MOVE",0
 .db "SOFT DROP",0
 .db "HARD DROP",0
 .db "LEFT ROTATE",0
 .db "RIGHT ROTATE",0
 .db "HOLD",0
 .db "PAUSE",0
remapMenuJumps:
 jp controlMenu
 ld a,0
 jr mapButton
 ld a,4
 jr mapButton
 ld a,8
 jr mapButton
 ld a,12
 jr mapButton
 ld a,16
 jr mapButton
 ld a,20
 jr mapButton
 ld a,24
 jr mapButton
 ld a,28
 jr mapButton

mapButton:
 push af
 ld ix, buttonPrompt
 res redrawObjBit,(ix+iDataType)
 call jptDrawObject
 call jptSwapVRamPTR
 pop af
 
 ld hl,buttonData
 ld de,0
 ld e,a
 add hl,de ;ptr to actual data to replace
 push hl
waitNoKey:
 call jptCheckKey
 cp -1
 jr nz, waitNoKey 
waitAnyKey:
 call jptCheckKey
 cp -1
 jr z, waitAnyKey
 ;a is keyid
 pop hl
 ld (hl),a
 
 jp buttonMappingMenu
 
;this draws all of the current button mappings.
;ix: points to temptext
listButtonMaps:
 ld b,8
 ld hl,buttonData
 ld ix,tempText
listButtonLoop:
 push bc
 push hl
 ld a,(hl) ;current buttonID
 ld c,a
 ld a,8
 sub b ;8-b = y location
 add a,a
 add a,a
 add a,a
 add a,8
 
 ld (ix+iDataY),a
 ld a,c ;restore buttonID
 
 ld hl,buttonText
 call jptGetStringInList
 ld (ix+iDataPTRH),h
 ld (ix+iDataPTRL),l

 res redrawObjBit, (ix+iDataType)
 push ix
 call jptDrawObject
 pop ix
 pop hl
 inc hl
 inc hl
 inc hl
 inc hl
 pop bc
 djnz listButtonLoop
 ret
 
;note: cursorID does not apply
;if active menu is not called
;so, typeMenu can also simply
;draw a lot of text in a column
;with not much extra data

pauseData:
 .db 2
;background box
 .db typeBox
 .dw 32
 .db 108
 .db boxColor
 .dw 56
 .db boxColor2, 24
;menu text
 .db typeMenu
 .dw 36
 .db 112
 .db textColor
 .dw pauseText - SSS
 .db 2, 2
;numbers etc
 
pauseText:
 .db " GAME ",0
 .db "PAUSED",0
 
gameOverData:
 .db 2
;background box
 .db typeBox
 .dw 32
 .db 108
 .db boxColor
 .dw 56
 .db boxColor2, 24
;menu text
 .db typeMenu
 .dw 44
 .db 112
 .db textColor
 .dw gameOverText - SSS
 .db 2, 2
;numbers etc
 
gameOverText:
 .db "GAME",0
 .db "OVER",0
 
buttonText:
;g1
 .db "GRAPH",0
 .db "TRACE",0
 .db "ZOOM",0
 .db "WINDOW",0
 .db "Y=",0
 .db "2ND",0
 .db "MODE",0
 .db "DEL",0
;g2
 .db "none",0 ;really ON: unobtainable
 .db "STO",0
 .db "LN",0
 .db "LOG",0
 .db "X^2",0
 .db "X^-1",0
 .db "MATH",0
 .db "ALPHA",0
;g3
 .db "0",0
 .db "1",0
 .db "4",0
 .db "7",0
 .db ",",0
 .db "SIN",0
 .db "APPS",0
 .db "XT0N",0
;g4
 .db ".",0
 .db "2",0
 .db "5",0
 .db "8",0
 .db "(",0
 .db "COS",0
 .db "PRGM",0
 .db "STAT",0
;g5
 .db "(-)",0
 .db "3",0
 .db "6",0
 .db "9",0
 .db "(",0
 .db "TAN",0
 .db "VARS",0
 .db "none",0
;g6
 .db "ENTER",0
 .db "+",0
 .db "-",0
 .db "*",0
 .db "/",0
 .db "^",0
 .db "CLEAR",0
 .db "none",0
;g7
 .db "DOWN",0
 .db "LEFT",0
 .db "RIGHT",0
 .db "UP",0

blockGraphicData:
 .db  0, 4
 .db  0, 8
 .db  0, 12
 .db  0, 16
 .db  0, 20
 .db  0, 24
 .db  0, 28
 .db  0, 31
 
spriteData:
.db sp2bpp
;default
.db $55,$55,$56
.db $55,$55,$5b
.db $5a,$aa,$af
.db $5a,$aa,$af
.db $5a,$aa,$af
.db $5a,$aa,$af
.db $5a,$aa,$af
.db $5a,$aa,$af
.db $5a,$aa,$af
.db $5a,$aa,$af
.db $6f,$ff,$ff
.db $bf,$ff,$ff

;wires
.db $00,$00,$00
.db $15,$55,$54
.db $10,$00,$04
.db $12,$aa,$84
.db $12,$00,$84
.db $12,$3c,$84
.db $12,$3c,$84
.db $12,$00,$84
.db $12,$aa,$84
.db $10,$00,$04
.db $15,$55,$54
.db $00,$00,$00

;square
.db $00,$00,$00
.db $15,$55,$54
.db $1a,$aa,$a4
.db $1b,$ff,$e4
.db $1b,$ff,$e4
.db $1b,$ff,$e4
.db $1b,$ff,$e4
.db $1b,$ff,$e4
.db $1b,$ff,$e4
.db $1a,$aa,$a4
.db $15,$55,$54
.db $00,$00,$00

;rounded
.db $05,$55,$50
.db $15,$6a,$ac
.db $56,$aa,$af
.db $5a,$aa,$af
.db $5a,$aa,$af
.db $6a,$aa,$af
.db $6a,$aa,$af
.db $6a,$aa,$bf
.db $6a,$aa,$bf
.db $6a,$ab,$ff
.db $3f,$ff,$fc
.db $0f,$ff,$f0

logoSprite:
;.db sp1bpp
;unneeded because calls to drawSpriteObj only
;rely on the type sent during the call
.db $3f,$df,$f8,$00,$00,$00,$00
.db $7f,$df,$f8,$00,$00,$00,$00
.db $ff,$df,$f8,$00,$00,$00,$00
.db $ff,$df,$f8,$60,$00,$c0,$00
.db $f0,$1e,$00,$60,$00,$c0,$00
.db $f0,$1f,$e0,$60,$00,$00,$00
.db $f0,$1f,$e3,$fc,$fe,$cf,$e0
.db $f0,$1f,$e3,$fd,$fe,$df,$e0
.db $f0,$1f,$e0,$61,$80,$d8,$00
.db $f0,$1e,$00,$61,$80,$df,$c0
.db $ff,$df,$f8,$61,$80,$cf,$e0
.db $ff,$df,$f8,$61,$80,$c0,$60
.db $7f,$df,$f8,$61,$80,$df,$e0
.db $3f,$df,$f8,$61,$80,$df,$c0

fontData:
;bits per pixel
.db sp1bpp
;actual font data
.db $00
.db $00
.db $00
.db $00
.db $00
.db $00
.db $00
.db $00
.db $00
.db $18
.db $18
.db $18
.db $18
.db $00
.db $18
.db $00
.db $00
.db $6c
.db $6c
.db $24
.db $00
.db $00
.db $00
.db $00
.db $00
.db $6c
.db $7e
.db $6c
.db $6c
.db $7e
.db $6c
.db $00
.db $18
.db $3c
.db $5a
.db $38
.db $1c
.db $5a
.db $3c
.db $18
.db $00
.db $66
.db $6c
.db $18
.db $18
.db $36
.db $66
.db $00
.db $00
.db $38
.db $6c
.db $38
.db $6a
.db $6c
.db $3a
.db $00
.db $00
.db $18
.db $18
.db $08
.db $00
.db $00
.db $00
.db $00
.db $00
.db $18
.db $30
.db $30
.db $30
.db $30
.db $18
.db $00
.db $00
.db $18
.db $0c
.db $0c
.db $0c
.db $0c
.db $18
.db $00
.db $00
.db $66
.db $3c
.db $7e
.db $7e
.db $3c
.db $66
.db $00
.db $00
.db $18
.db $18
.db $7e
.db $7e
.db $18
.db $18
.db $00
.db $00
.db $00
.db $00
.db $00
.db $18
.db $18
.db $10
.db $00
.db $00
.db $00
.db $00
.db $7e
.db $7e
.db $00
.db $00
.db $00
.db $00
.db $00
.db $00
.db $00
.db $00
.db $18
.db $18
.db $00
.db $00
.db $06
.db $0c
.db $18
.db $18
.db $30
.db $60
.db $00
numbers:
.db $00
.db $3c
.db $66
.db $66
.db $66
.db $66
.db $3c
.db $00
.db $00
.db $18
.db $78
.db $18
.db $18
.db $18
.db $7e
.db $00
.db $00
.db $3c
.db $66
.db $0c
.db $18
.db $30
.db $7e
.db $00
.db $00
.db $3c
.db $66
.db $1c
.db $06
.db $66
.db $3c
.db $00
.db $00
.db $66
.db $66
.db $7e
.db $06
.db $06
.db $06
.db $00
.db $00
.db $7e
.db $60
.db $7c
.db $06
.db $06
.db $7c
.db $00
.db $00
.db $3c
.db $60
.db $7c
.db $66
.db $66
.db $3c
.db $00
.db $00
.db $7e
.db $06
.db $06
.db $0c
.db $0c
.db $0c
.db $00
.db $00
.db $3c
.db $66
.db $3c
.db $7e
.db $66
.db $3c
.db $00
.db $00
.db $3c
.db $66
.db $66
.db $3e
.db $06
.db $3c
.db $00
.db $00
.db $18
.db $18
.db $00
.db $00
.db $18
.db $18
.db $00
.db $00
.db $18
.db $18
.db $00
.db $18
.db $18
.db $10
.db $00
.db $00
.db $06
.db $1e
.db $78
.db $78
.db $1e
.db $06
.db $00
.db $00
.db $7e
.db $7e
.db $00
.db $00
.db $7e
.db $7e
.db $00
.db $00
.db $60
.db $78
.db $1e
.db $1e
.db $78
.db $60
.db $00
.db $00
.db $3c
.db $66
.db $06
.db $1c
.db $00
.db $18
.db $00
.db $00
.db $3c
.db $66
.db $6e
.db $6a
.db $64
.db $30
.db $00
.db $00
.db $3c
.db $66
.db $7e
.db $66
.db $66
.db $66
.db $00
.db $00
.db $7c
.db $66
.db $7c
.db $66
.db $66
.db $7c
.db $00
.db $00
.db $3c
.db $66
.db $60
.db $60
.db $66
.db $3c
.db $00
.db $00
.db $7c
.db $66
.db $66
.db $66
.db $66
.db $7c
.db $00
.db $00
.db $7e
.db $60
.db $78
.db $60
.db $60
.db $7e
.db $00
.db $00
.db $7e
.db $60
.db $78
.db $60
.db $60
.db $60
.db $00
.db $00
.db $3c
.db $66
.db $60
.db $6e
.db $66
.db $3c
.db $00
.db $00
.db $66
.db $66
.db $7e
.db $66
.db $66
.db $66
.db $00
.db $00
.db $7e
.db $18
.db $18
.db $18
.db $18
.db $7e
.db $00
.db $00
.db $1e
.db $06
.db $06
.db $06
.db $66
.db $3c
.db $00
.db $00
.db $66
.db $6c
.db $78
.db $6c
.db $66
.db $66
.db $00
.db $00
.db $60
.db $60
.db $60
.db $60
.db $60
.db $7e
.db $00
.db $00
.db $66
.db $7e
.db $7e
.db $7e
.db $66
.db $66
.db $00
.db $00
.db $66
.db $76
.db $7e
.db $6e
.db $66
.db $66
.db $00
.db $00
.db $3c
.db $66
.db $66
.db $66
.db $66
.db $3c
.db $00
.db $00
.db $7c
.db $66
.db $7c
.db $60
.db $60
.db $60
.db $00
.db $00
.db $3c
.db $66
.db $66
.db $66
.db $7c
.db $3e
.db $00
.db $00
.db $7c
.db $66
.db $7c
.db $66
.db $66
.db $66
.db $00
.db $00
.db $3c
.db $60
.db $3c
.db $06
.db $66
.db $3c
.db $00
.db $00
.db $7e
.db $18
.db $18
.db $18
.db $18
.db $18
.db $00
.db $00
.db $66
.db $66
.db $66
.db $66
.db $66
.db $3c
.db $00
.db $00
.db $66
.db $66
.db $3c
.db $3c
.db $18
.db $18
.db $00
.db $00
.db $66
.db $66
.db $66
.db $7e
.db $7e
.db $66
.db $00
.db $00
.db $66
.db $3c
.db $18
.db $18
.db $3c
.db $66
.db $00
.db $00
.db $66
.db $66
.db $3c
.db $18
.db $18
.db $18
.db $00
.db $00
.db $7e
.db $06
.db $1c
.db $38
.db $60
.db $7e
.db $00
.db $00
.db $1c
.db $18
.db $18
.db $18
.db $18
.db $1c
.db $00
.db $00
.db $60
.db $30
.db $18
.db $18
.db $0c
.db $06
.db $00
.db $00
.db $38
.db $18
.db $18
.db $18
.db $18
.db $38
.db $00
.db $00
.db $18
.db $18
.db $3c
.db $3c
.db $66
.db $66
.db $00
.db $00
.db $00
.db $00
.db $00
.db $00
.db $00
.db $7e
.db $00
.db $00
.db $30
.db $18
.db $0c
.db $00
.db $00
.db $00
.db $00
.db $00
.db $00
.db $3c
.db $06
.db $3e
.db $66
.db $3e
.db $00
.db $00
.db $60
.db $60
.db $7c
.db $66
.db $66
.db $7c
.db $00
.db $00
.db $00
.db $3c
.db $66
.db $60
.db $66
.db $3c
.db $00
.db $00
.db $06
.db $06
.db $3e
.db $66
.db $66
.db $3e
.db $00
.db $00
.db $00
.db $3c
.db $66
.db $7e
.db $60
.db $3c
.db $00
.db $00
.db $0e
.db $18
.db $7e
.db $18
.db $18
.db $18
.db $00
.db $00
.db $3e
.db $66
.db $66
.db $3e
.db $06
.db $3c
.db $00
.db $00
.db $60
.db $60
.db $7c
.db $66
.db $66
.db $66
.db $00
.db $00
.db $18
.db $00
.db $18
.db $18
.db $18
.db $18
.db $00
.db $00
.db $18
.db $00
.db $18
.db $18
.db $18
.db $70
.db $00
.db $00
.db $60
.db $66
.db $6c
.db $78
.db $6c
.db $66
.db $00
.db $00
.db $18
.db $18
.db $18
.db $18
.db $18
.db $18
.db $00
.db $00
.db $00
.db $7c
.db $7e
.db $7e
.db $7e
.db $66
.db $00
.db $00
.db $00
.db $7c
.db $66
.db $66
.db $66
.db $66
.db $00
.db $00
.db $00
.db $3c
.db $66
.db $66
.db $66
.db $3c
.db $00
.db $00
.db $7c
.db $66
.db $66
.db $7c
.db $60
.db $60
.db $00
.db $00
.db $3e
.db $66
.db $66
.db $3e
.db $06
.db $06
.db $00
.db $00
.db $00
.db $7c
.db $66
.db $60
.db $60
.db $60
.db $00
.db $00
.db $00
.db $3c
.db $60
.db $3c
.db $06
.db $7c
.db $00
.db $00
.db $18
.db $18
.db $7e
.db $18
.db $18
.db $18
.db $00
.db $00
.db $00
.db $66
.db $66
.db $66
.db $66
.db $3e
.db $00
.db $00
.db $00
.db $66
.db $66
.db $66
.db $3c
.db $18
.db $00
.db $00
.db $00
.db $66
.db $7e
.db $7e
.db $7e
.db $3e
.db $00
.db $00
.db $00
.db $66
.db $3c
.db $18
.db $3c
.db $66
.db $00
.db $00
.db $00
.db $66
.db $66
.db $3e
.db $06
.db $3c
.db $00
.db $00
.db $00
.db $7e
.db $0c
.db $18
.db $30
.db $7e
.db $00
.db $00
.db $0e
.db $18
.db $70
.db $70
.db $18
.db $0e
.db $00
.db $00
.db $18
.db $18
.db $18
.db $18
.db $18
.db $18
.db $00
.db $00
.db $70
.db $18
.db $0e
.db $0e
.db $18
.db $70
.db $00
.db $00
.db $00
.db $30
.db $5a
.db $0c
.db $00
.db $00
.db $00
.db $00
.db $10
.db $20
.db $7e
.db $20
.db $10
.db $00
.db $00
fontDataEnd:

;dw $00ff, $0ff0, $ff00, $f00f
paletteData:
.dw $7fff, $1CE7, $3def, $0000
.dw $7fff, $4f7d, $029d, $1d39
.dw $7fff, $3a57, $1d39, $0000
.dw $7fff, $7f21, $7de4, $4402 ;4
.dw $7fff, $7fff, $7fc0, $7f21
.dw $7fff, $5b83, $12c9, $3def
.dw $7fff, $66fc, $5134, $0000
.dw $7fff, $7eb9, $7464, $4402 ;8
.dw $3def, $0c63, $1ce7, $0000
.dw $3def, $25ae, $014e, $0c8c
.dw $3def, $1d2b, $0c8c, $0000
.dw $3def, $3d80, $3ce2, $2001 ;12
.dw $3def, $3def, $3de0, $3d80
.dw $3def, $2dc1, $0964, $1ce7
.dw $3def, $316e, $288a, $0000
.dw $3def, $3d4c, $3822, $2001 ;16
paletteDataEnd: