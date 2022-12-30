#include "includes\ti84pce.inc"
#include "includes\tetrice.inc"

.assume ADL=1
.org saveSScreen

dataReferences:
.dl spriteData ;std sprites like blocks, field, etc
.dl fontData ;font data. 
.dl 0 ;previously block sprite+palette pairs
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
.dl fieldInfo
.dl holdInfo
.dl levelInfo
.dl scoreInfo
.dl linesInfo
.dl previewInfo
.dl timerInfo


;note: address expected in tetrice.inc to be refSize * 16 + SSS. be sure to modify if refs are added.
initDat:
 call applyTheme
 ret

boxColor = 14
boxColor2= 15
textColor= 3
infoBoxX = 168
infoBoxY = 16

uiCompound:
;backgroundInfo:
.db typeMap
.dw 0
.db 0
.db 16
.dl 1
.db 20, 15 ;size of entire screen
;backgroundInfoExtended:
.db typeExtended
.dl bgSprite
.dl bgTiles
.dl empty300
;bg box for logo
.db typeBox
.dw 248
.db 192
.db 2
.dl 72
.db 3, 38
;logo
.db typeSprite1bpp
.dw 256
.db 200
.db 1
.dl logoSprite 
.db 7, 14
;version string
.db typeString
.dw -versionStringSize * 8 + 320
.db 216
.db textColor
.dl versionString
.db 0, 0

;uiCompoundObj
;.db typeCompound
;.dw 0, 0
;.dl uiCompound
;.db 4, 0

;Game items info
itemsInfo:
.db 12 ;number of items
;uiCompoundObj
.db typeCompound
.dw 0, 0
.dl uiCompound
.db 4, 0
fieldInfo:
.db typeMap
.dw 0 ;x
.db 0 ;y
.db 12 ;a/size of tile
.dl 1 ;flags
.db fieldDrawWidth, fieldDrawHeight ;10, 20 default
;fieldExtended
.db typeExtended ;part of previous object
.dl spriteData
.dl blockGraphicData ;tileset pointer here
.dl (fieldHeight - fieldDrawHeight) * fieldDrawWidth + field
holdInfo:
.db typeCompound ;typeHold
.dw 0 ;unused
.db 0 ;unused
.db 0 ;unused
.dl holdCompound ;ptr to compound object data (does not include item count)
.db 3, 0 ; number of items, unused
previewInfo:
.db typeCompound
.dw 0 ;unused
.db 0 ;unused
.db 0 ;unused
.dl previewCompound 
.db 7, 0 ; number of items, unused
;BACKGROUND BOX INFO
.db typeBox
.dw infoBoxX ;x
.db infoBoxY ;y
.db boxColor ;color of main
.dl 128 ;width
.db boxColor2, 80 ;bordercolor, height
;SCORE TEXT ETC.
.db typeMenu
.dw infoBoxX + 8
.db infoBoxY + 8
.db textColor
.dl gameText
.db 7, 0 ;cursorid doesn't matter cause not a active menu
scoreInfo:
.db typeNumber
.dw infoBoxX + 8 + 48
.db infoBoxY + 8 + 0
.db textColor
.dl score ;variables are saved in PSS
.db 8, boxColor ; size of number, bgcolor
levelInfo:
.db typeNumber
.dw infoBoxX + 8 + 48
.db infoBoxY + 8 + 24
.db textColor
.dl level
.db 3, boxColor ;size number, bg color
linesInfo:
.db typeNumber
.dw infoBoxX + 8 + 48
.db infoBoxY + 8 + 32
.db textColor
.dl lines
.db 4, boxcolor ;number of digits, bgcolor
timerInfo:
.db typeNumber
.dw infoBoxX + 8 + 48
.db infoBoxY + 8 + 8
.db textColor
.dl globalTimer
.db 8, boxcolor
highscoreInfo:
.db typeNumber
.dw infoBoxX + 8 + 48
.db infoBoxY + 8 + 48
.db textColor
.dl highScore
.db 8, boxcolor

holdCompound:
holdBackground:
.db typeMap
.dw 132 ;x
.db 160 ;y
.db 12 ;tile size (pixels)
.dl 0 ;no flags needed
.db 4, 4 ;width, height (tiles)
holdBGExtended:
.db typeExtended
.dl spriteData
.dl blockGraphicData
.dl empty300
holdMino:
.db typeCustom
.dw 144 ;x
.db 184 ;y
.db holdTOfs ;minoTypes offset
.dl drawMinoFromTypeWrapper
.db 1<<csUsedHold, 0 ;mask against csStatus, setting (partially overrode in draw function)

previewCompound:
previewBox:
 .db typeBox
 .dw 126 ;x
 .db 6 ;y
 .db 2 ;color
 .dl 36 ;width
 .db 3, 144 ;border, height
previewMino1:
 .db typeCustom
 .dw 138 ;x
 .db 18 ;y
 .db bag1Ofs + 0 ;first preview index
 .dl drawMinoFromTypeWrapper
 .db 0, 1<<drawMinoHalf
previewMino2:
 .db typeCustom
 .dw 138 ;x
 .db 44 ;y
 .db bag1Ofs + 1 ;second preview piece, etc.
 .dl drawMinoFromTypeWrapper
 .db 0, 1<<drawMinoHalf
previewMino3:
 .db typeCustom
 .dw 138 ;x
 .db 68 ;y
 .db bag1Ofs + 2
 .dl drawMinoFromTypeWrapper
 .db 0, 1<<drawMinoHalf
previewMino4:
 .db typeCustom
 .dw 138 ;x
 .db 90 ;y
 .db bag1Ofs + 3
 .dl drawMinoFromTypeWrapper
 .db 0, 1<<drawMinoHalf
previewMino5:
 .db typeCustom
 .dw 138 ;x
 .db 116 ;y
 .db bag1Ofs + 4
 .dl drawMinoFromTypeWrapper
 .db 0, 1<<drawMinoHalf
previewMino6:
 .db typeCustom
 .dw 138 ;x
 .db 138 ;y
 .db bag1Ofs + 5
 .dl drawMinoFromTypeWrapper
 .db 0, 1<<drawMinoHalf

;ix points to holdMino's data
drawMinoFromTypeWrapper:
 or a,a
 sbc hl,hl
 ld a,(curStatus)
 push hl
 push hl
 push hl
 pop bc
 ld c,(ix+iDataA)
 ld hl,minoTypes
 add hl,bc
 ld c,(hl)
 pop de ;0
 pop hl ;0
 ld e,(ix+iDataXL)
 ld d,(ix+iDataXH)
 ld h,(ix+iDataH)
 and (ix+iDataW)
 jr z, holdReady
 set drawMinoDark, h
holdReady:
 ld l,(ix+iDataY)
 ld a,c
 push ix
 ld ix,fieldInfo
 call jptDrawMino
 pop ix
 ret

gameText:
 .db "Score:",0
 .db "Timer:",0
 .db 0
 .db "Level:",0
 .db "Lines:",0
 .db 0
 .db " Best:",0
 

SSSInfo = itemsInfo

menuObjData:
 .db 4
;uiCompoundObj
 .db typeCompound
 .dw 0, 0
 .dl uiCompound
 .db 5, 0
 ;background
 .db typeBox
 .dw 0 ;x
 .db 0 ;y
 .db 5 ;color
 .dl 72 ;width
 .db 6, 40 ;bordercolor, height
 ;menu text
 .db typeMenu
 .dw 8
 .db 8
 .db textColor
 .dl menuText
 .db 3, 3 ;# items, cursorID within menuObjData
 ;cursor
 .db typeString
 .dw 0
 .db 8
 .db textColor
 .dl cursorString
 .db 0, 0

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
 .db 5
  ;background
 .db typeBox
 .dw 0 ;x
 .db 0 ;y
 .db 2 ;color
 .dl 200 ;width
 .db 1, 48 ;bordercolor, height
 ;menu text
 .db typeMenu
 .dw 8
 .db 8
 .db textColor
 .dl startMenuText
 .db 3, 2 ;# items, cursorID within menuObjData
 ;cursor
 .db typeString
 .dw 0
 .db 8
 .db textColor
 .dl cursorString
 .db 0, 0
startMenuSelectMode:
 .db typeString
 .dw 48
 .db 8
 .db textColor
 .dl modeText
 .db 0, 2 ;bg color
startMenuSelectLev:
 .db typeNumber
 .dw 56
 .db 16
 .db textColor
 .dl level
 .db 2, 2 ;digits, bgcolor
 
startMenuText:
 .db "Mode:",0
 .db "Level:",0
 .db "BEGIN",0
startMenuJumps:
 jp jptMainMenu ;example of prev menu jump
 jp selectMode
 jp selectLev
 jp jptInitGame ;starts game
 
selectLev:
 ld ix, startMenuSelectLev
 ld a, 51
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
 .dl 160 ;width
 .db 10, 184 ;bordercolor, height
 ;menu text
 .db typeMenu
 .dw 8
 .db 8
 .db textColor
 .dl modeText
 .db 21, 2 ;# items, cursorID within menuObjData
 ;cursor
 .db typeString
 .dw 0
 .db 8
 .db textColor
 .dl cursorString
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
.db "ULTRA-1",0
.db "ULTRA-3",0
.db "ULTRA-5",0
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
 ld a,1
 jr setUltra 
 ld a,3
 jr setUltra
 ld a,5
 jr setUltra

setMarathon:
 ld e,0
 call setModeFromE
 jr setLines
 
setRetro:
 ld e,1*modeSize
 call setModeFromE
 jr setLines

setLineRace:
 ld e,2*modeSize
 call setModeFromE
 jr setLines
 
setDig:
 ld e,3*modeSize
 call setModeFromE
 jr setGeneration

setExcavate: 
 ld e,3*modeSize
 call setModeFromE
 jr setGenGGD

setRising:
 ld e,4*modeSize
 call setModeFromE
 jr setRisingInfo
 
setCascade:
 ld e,5*modeSize
 call setModeFromE
 jr setLines
 
setUltra:
 ld e,6*modeSize
 call setModeFromE
 ld l,a  ;minutes
 ld h,30 ;30*2 sec
 mlt hl  ;min * 60 sec
 ld h,60 ;approx. frame/sec
 mlt hl  ;min * 60sec/min * 60frame/sec 
 add hl,hl ;this is the *2
 ld (globalTimer),hl
 jr slToMenu

setLines:
 or a,a
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
 ld a,e
 rlca
 rlca
 rlca 
 and $07
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
 .db rMarathon, rRising, rNull, rLine ;dig challenge
 .db rMarathon, rCascadeExtra, rLines, rScore ;cascade
 .db rMarathon, rNull, rCountdown, rScore ;ultra
modeSize=4

setModeFromE:
 or a,a
 sbc hl,hl
 push hl
 ld l,e
 ld de,modeData
 add hl,de
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
 .dl 80 ;width
 .db 19, 40 ;bordercolor, height
;heading
 .db typeString
 .dw 0
 .db 8
 .db textColor
 .dl optionsText
 .db 0, 0
;menu
 .db typeMenu
 .dw 8
 .db 16
 .db textColor
 .dl optionsMenuText
 .db 5, 3
;curosr
 .db typeString
 .dw 0
 .db 16
 .db textColor
 .dl cursorString
 .db 0, 0
;themeSelect
themeSelection:
 .db typeCompound
 .dw 0
 .db 0
 .db 0
 .dl themeSelectionCompound
 .db 7, 0

;this allows for live preview using the compound as the selectNumber redraw object
themeSelectionCompound:
blockSelectNum:
 .db typeNumber8
 .dw 56
 .db 16
 .db textColor
 .dl themeBlock
 .db 3, 18
colorSelectNum:
 .db typeNumber8
 .dw 56
 .db 24
 .db textColor
 .dl themeColor
 .db 3, 18
bgSelectNum:
 .db typeNumber8
 .dw 56
 .db 32
 .db textColor
 .dl themeBack
 .db 3, 18
bgColSelectNum:
 .db typeNumber8
 .dw 56
 .db 40
 .db textColor
 .dl themeBGColor
 .db 3, 18
setThemeLive:
 .db typeCustom
 .dw 0,0
 .dl applyTheme
 .db 0,0 
previewMapInfo:
 .db typeMap
 .dw 100 ;x
 .db 144 ;y
 .db 12 ;a/size of tile
 .dl 1 ;flags
 .db fieldDrawWidth, 8 ;10, 20 default
;fieldExtended
 .db typeExtended ;part of previous object
 .dl spriteData
 .dl blockGraphicData ;tileset pointer here
 .dl previewMap

;previewBGBox:
; .db typeBox
; .dw 96
; .db 18
; .db 2
; .dl 64
; .db 3, 176
;preview
;previewThemeMino:
; .db typeCustom
; .dw 116
; .db 80  ;will be overrode by drawPreviewMino
; .db demoTOfs
; .dl drawPreviewMinos
; .db 0, 0
 
;drawPreviewMinos:
; call applyTheme
; ld b,7
;dPMLoop:
; push bc
; ld a,b
; ld (demoT),a
; ld c,24
; mlt bc
; ld a,c
; ld (previewThemeMino+iDataY),a
;at the time of writing ix is preserved by dMFTW
; call drawMinoFromTypeWrapper
; pop bc
; djnz dPMLoop
; ret

optionsMenuText:
 .db "BLOCK:",0
 .db "COLOR:",0
 .db "BACKG:",0
 .db "BGCOL:",0
 .db "CONTROLS",0
optionJumps:
 jp jptMainMenu
 jp setTheme
 jp setColor
 jp setBack
 jp setBackColor
 jp controlMenu

controlMenu:
 ld ix,controlMenuData
 jp jptActiveMenu
 
setTheme:
 ld a,12
 ld hl,themeBlock
sharedTheme:
 ld ix,themeSelection
 call jptSetNumber
 
 call applyTheme
 
 ld ix, optionsMenuData
 jp jptActiveMenu ;back to options menu
 
setColor:
 ld a,4
 ld hl,themeColor
 jr sharedTheme

setBack:
 ld a,4
 ld hl,themeBack
 jr sharedTheme

setBackColor:
 ld a,64
 ld hl,themeBGColor
 jr sharedTheme

applyTheme:
 ld hl,themeBlock
 ld e,(hl) ;block
 inc hl
 ld b,(hl) ;color flags
 inc hl
 ld d,(hl) ;background tile
 inc hl
 ld a,(hl) ;bg color
 ld hl,bgTiles
 ld (hl),d
 inc hl
 ld (hl),a
;check dark
 srl b
 ld hl,$e77fff
 jr nc,noSetDark
 ld hl,$e72108 ;first 3 bytes of palette
noSetDark:
 ld ($e30200),hl
;check mono
 srl b
 ld hl,blockGraphicData
 jr nc,noSetMono
 ld hl,monoGraphicData
noSetMono:
 ld (holdBackground+iExtTileset),hl
 ld (fieldInfo+iExtTileset),hl
 ld (previewMapInfo+iExtTileset),hl
 
 inc hl
 inc hl ;skip the null block
 ld b,7
replaceBlockSpriteID:
 ld (hl),e
 inc hl ;palette
 inc hl ;next block spriteid
 djnz replaceBlockSpriteID
 ;all spriteid has been replaced, back to option
 ret

ctrlMenuTextY=8
ctrlColumnX1=8
ctrlColumnX2=128
controlMenuData:
 .db 10
;background
 .db typeBox
 .dw 0
 .db 0
 .db 22
 .dl 172
 .db 23, 208
;menu
 .db typeMenu
 .dw ctrlColumnX1
 .db ctrlMenuTextY
 .db textColor
 .dl controlMenuText
 .db 22, 2
;cursor
 .db typeString
 .dw 0
 .db ctrlMenuTextY
 .db textColor
 .dl cursorString 
 .db 0, 0
keyRepeatStart:
 .db typeNumber8
 .dw ctrlColumnX2
 .db ctrlMenuTextY
 .db textColor
 .dl userDASDelay
 .db 2, 22 ;digits, bgcolor
keyRepeatLR:
 .db typeNumber8
 .dw ctrlColumnX2
 .db ctrlMenuTextY + 8
 .db textColor
 .dl buttonLeft + buttonTimeRepeat
 .db 2, 22 ;digits, bgcolor
keyRepeatDrop:
 .db typeNumber8
 .dw ctrlColumnX2
 .db ctrlMenuTextY + 16
 .db textColor
 .dl buttonSoft + buttonTimeRepeat
 .db 2, 22 ;digits, bgcolor
lockDelayNum:
 .db typeNumber8
 .dw ctrlColumnX2
 .db ctrlMenuTextY + 24
 .db textColor
 .dl userLockDelay
 .db 2, 22 ;digits, bgcolor
spawnDelayNum:
 .db typeNumber8
 .dw ctrlColumnX2
 .db ctrlMenuTextY + 32
 .db textColor
 .dl userSpawnDelay
 .db 2, 22 ;digits, bgcolor
;button info
 .db typeCustom
 .dw ctrlColumnX2
 .db 56
 .db 0
 .dl listButtonMaps
 .db 8, 8
;menu button info
 .db typeCustom
 .dw ctrlColumnX2
 .db 128
 .db 0
 .dl listMenuButtonMaps
 .db 7, 8

buttonPrompt:
 .db typeString
 .dw 8
 .db 192
 .db textColor
 .dl buttonPromptText 
 .db 0, 0
buttonPromptText:
 .db "PRESS BUTTON TO SET",0
 
tempText:
 .db typeString
 .dw 128
 .db 0
 .db textColor
 .dl 0
 .db 0, 0
 
controlMenuText:
 .db "DAS START",0
 .db "DAS REPEAT",0
 .db "DROP REPEAT",0
 .db "LOCK DELAY",0
 .db "SPAWN DELAY",0
 .db 0
;remapMenuText:
 .db "LEFT MOVE",0
 .db "RIGHT MOVE",0
 .db "SOFT DROP",0
 .db "HARD DROP",0
 .db "LEFT ROTATE",0
 .db "RIGHT ROTATE",0
 .db "HOLD",0
 .db "PAUSE",0
 .db 0
 .db "MENU UP",0
 .db "MENU DOWN",0
 .db "MENU LEFT",0
 .db "MENU RIGHT",0
 .db "MENU CONFIRM",0
 .db "MENU BACK",0
 .db "EXIT",0
controlJumps:
 jp gotoOptions
 jp setDASDelay
 jp setButtonInfoLR
 jp setButtonInfoDrop
 jp setLockDelay
 jp setSpawnDelay
 .dw 0,0
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
 .dw 0,0
 ld a,32
 jr mapButton
 ld a,36
 jr mapButton
 ld a,40
 jr mapButton
 ld a,44
 jr mapButton
 ld a,48
 jr mapButton
 ld a,52
 jr mapButton
 ld a,56
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
 
 jp controlMenu
 
setDASDelay:
 ld ix, keyRepeatStart
 ld a,31
 call jptSetNumber
 jp controlMenu

setLockDelay:
 ld ix, lockDelayNum
 ld a, 61
 call jptSetNumber
 jp controlMenu

setSpawnDelay:
 ld ix, spawnDelayNum
 ld a,31
 call jptSetNumber
 jp controlMenu

setButtonInfoLR:
 ld ix, keyRepeatLR

 ld a,100
 push ix
 call jptSetNumber
 pop ix 
 
 ;affect both LEFT and RIGHT.
 ld a,(numberSelection)
 ld hl,(ix+iDataPTR)
 inc hl
 inc hl
 inc hl
 inc hl
 ld (hl),a
 
 jp controlMenu
 
setButtonInfoDrop:
 ld ix, keyRepeatDrop
 ld a,100
 call jptSetNumber

 jp controlMenu

;this draws all of the current button mappings.
;ix: points to temptext
listMenuButtonMaps:
 ld hl,menuButtonData
 jr listButtonShared 
listButtonMaps:
 ld hl,buttonData
listButtonShared:
 ld de,(ix+iDataX)
 ld b,(ix+iDataW)
 ld (tempText+iDataX),de ;overwrites Y as well, but Y is re-written in loop so it's fine
listButtonLoop:
 push bc
 push hl
 ld a,(ix+iDataW)
 sub b ;8-b = y location
 add a,a
 add a,a
 add a,a ;*char size [8]
 add a,(ix+iDataY) ;add y offset
 ld (tempText+iDataY),a
 
 ld a,(hl) ;current buttonId
 
 ld hl,buttonText
 call jptGetStringInList
 ld (tempText+iDataPTRL),hl

 push ix
 ld ix,tempText
 res redrawObjBit, (ix+iDataType)
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
 .dl 56
 .db boxColor2, 24
;menu text
 .db typeMenu
 .dw 36
 .db 112
 .db textColor
 .dl pauseText
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
 .dl 56
 .db boxColor2, 24
;menu text
 .db typeMenu
 .dw 44
 .db 112
 .db textColor
 .dl gameOverText
 .db 2, 2
;numbers etc
 
gameOverText:
 .db "GAME",0
 .db "OVER",0
 
buttonText:
;g1
 .db "F5",0
 .db "F4",0
 .db "F3",0
 .db "F2",0
 .db "F1",0
 .db "2ND",0
 .db "MODE",0
 .db "DEL",0
;g2
 .db "ON",0 ;unobtainable
 .db "X",0
 .db "S",0
 .db "N",0
 .db "I",0
 .db "D",0
 .db "A",0
 .db "ALPHA",0
;g3
 .db "SPACE",0
 .db "Y",0
 .db "R",0
 .db "O",0
 .db "J",0
 .db "E",0
 .db "B",0
 .db "XT0N",0
;g4
 .db ":",0
 .db "Z",0
 .db "U",0
 .db "P",0
 .db "K",0
 .db "F",0
 .db "C",0
 .db "STAT",0
;g5
 .db "?",0
 .db "THETA",0
 .db "V",0
 .db "Q",0
 .db "L",0
 .db "G",0
 .db "VARS",0
 .db "NA",0
;g6
 .db "ENTER",0
 .db "\"",0
 .db "W",0
 .db "R",0
 .db "M",0
 .db "H",0
 .db "CLEAR",0
 .db "NA",0
;g7
 .db "DOWN",0
 .db "LEFT",0
 .db "RIGHT",0
 .db "UP",0

blockGraphicData:
 .db  0, 0 ;null
 .db  0, 4 ;I
 .db  0, 8 ;L
 .db  0, 12 ;J
 .db  0, 16 ;O
 .db  0, 20 ;S
 .db  0, 24 ;T
 .db  0, 28 ;Z
 .db  0, 29 ;garbage
blockGraphicDataSize=$-blockGraphicData

monoGraphicData:
 .db  0, 0 ;null
 .db  0, 32 ;I
 .db  0, 32 ;L
 .db  0, 32 ;J
 .db  0, 32 ;O
 .db  0, 32 ;S
 .db  0, 32 ;T
 .db  0, 32 ;Z
 .db  0, 1 ;garbage

previewMap:
;10*8
.db 0,0,0,0,0,0,1,0,0,0
.db 0,0,0,0,0,0,1,0,0,0
.db 0,0,0,7,0,0,1,0,0,0
.db 2,0,7,7,5,0,1,0,4,4
.db 2,0,7,6,5,5,0,3,4,4
.db 2,2,6,6,6,5,0,3,3,3
.db 8,8,8,8,8,8,0,8,8,8
.db 8,8,8,8,8,8,0,8,8,8

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

;quads
.db $ff, $ff, $ff
.db $ea, $be, $ab
.db $ea, $be, $ab
.db $ea, $be, $ab
.db $ea, $be, $ab
.db $ff, $eb, $ff
.db $ff, $eb, $ff
.db $ea, $be, $ab
.db $ea, $be, $ab
.db $ea, $be, $ab
.db $ea, $be, $ab
.db $ff, $ff, $ff

;shine
.db $ff, $ff, $ff
.db $e9, $56, $6b
.db $e5, $59, $ab
.db $d5, $66, $ab
.db $d5, $9a, $ab
.db $d6, $6a, $ab
.db $d9, $aa, $ab
.db $e6, $aa, $ab
.db $da, $aa, $ab
.db $ea, $aa, $ab
.db $ea, $aa, $ab
.db $ff, $ff, $ff

;glow
.db $ff, $ff, $ff
.db $ff, $ff, $ff
.db $fa, $aa, $af
.db $fa, $aa, $af
.db $fa, $55, $af
.db $fa, $55, $af
.db $fa, $55, $af
.db $fa, $55, $af
.db $fa, $aa, $af
.db $fa, $aa, $af
.db $ff, $ff, $ff
.db $ff, $ff, $ff

;bubble
.db $0a, $aa, $a0
.db $29, $56, $a8
.db $a5, $56, $aa
.db $95, $aa, $aa
.db $96, $aa, $aa
.db $96, $aa, $aa
.db $aa, $aa, $aa
.db $aa, $aa, $aa
.db $aa, $aa, $aa
.db $aa, $aa, $ae
.db $2a, $aa, $f8
.db $0a, $aa, $a0

;window
.db $ff, $ff, $ff
.db $ea, $aa, $ab
.db $e5, $56, $ab
.db $e4, $06, $ab
.db $e4, $06, $ab
.db $e5, $56, $ab
.db $ea, $aa, $ab
.db $ea, $aa, $ab
.db $ea, $aa, $ab
.db $ea, $aa, $ab
.db $ea, $aa, $ab
.db $ff, $ff, $ff

;crate
.db $ff, $ff, $ff
.db $fa, $aa, $af
.db $ee, $aa, $bb
.db $eb, $aa, $eb
.db $ea, $eb, $ab
.db $ea, $be, $ab
.db $ea, $be, $ab
.db $ea, $eb, $ab
.db $eb, $aa, $eb
.db $ee, $aa, $bb
.db $fa, $aa, $af
.db $ff, $ff, $ff

;light
.db $2a, $aa, $a8
.db $95, $55, $56
.db $95, $55, $56
.db $95, $55, $56
.db $95, $55, $56
.db $95, $55, $56
.db $95, $55, $56
.db $95, $55, $56
.db $95, $55, $56
.db $95, $55, $56
.db $95, $55, $56
.db $2a, $aa, $a8

;puyo
.db $00, $aa, $00
.db $0a, $aa, $a0
.db $2a, $aa, $a8
.db $a5, $aa, $5a
.db $97, $eb, $d6
.db $97, $eb, $d6
.db $a5, $aa, $5a
.db $aa, $aa, $aa
.db $aa, $aa, $aa
.db $2a, $aa, $a8
.db $0a, $aa, $a0
.db $00, $aa, $00

bgTiles:
.db $00, $01

bgSprite:
.db sp1bpp
;diags
.db $33, $33, $66, $66, $cc, $cc, $99, $99
.db $33, $33, $66, $66, $cc, $cc, $99, $99
.db $33, $33, $66, $66, $cc, $cc, $99, $99
.db $33, $33, $66, $66, $cc, $cc, $99, $99
;block
.db $40, $02, $a0, $05, $50, $0b, $3f, $f7
.db $10, $0f, $10, $0f, $10, $0f, $10, $0f
.db $10, $0f, $10, $0f, $10, $0f, $10, $0f
.db $2f, $f7, $5f, $fb, $bf, $fd, $7f, $fe
;star
.db $01, $80, $01, $80, $01, $80, $02, $40
.db $02, $40, $04, $20, $19, $98, $e2, $47
.db $e2, $47, $19, $98, $04, $20, $02, $40
.db $02, $40, $01, $80, $01, $80, $01, $80
;????
.db $08, $08, $08, $08, $08, $08, $10, $14
.db $10, $14, $28, $22, $46, $41, $81, $80
.db $80, $40, $00, $b1, $01, $0a, $02, $04
.db $02, $04, $04, $04, $1c, $0a, $e4, $09

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

;sequence of 16 bytes of zeroes
;used to draw hold map background
;note: sp1bpp = 0

fontData:
;bits per pixel
.db sp1bpp
;actual font data
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $18, $18, $18, $18, $00, $18, $00
.db $00, $6c, $6c, $24, $00, $00, $00, $00
.db $00, $6c, $7e, $6c, $6c, $7e, $6c, $00
.db $18, $3c, $5a, $38, $1c, $5a, $3c, $18
.db $00, $66, $6c, $18, $18, $36, $66, $00
.db $00, $38, $6c, $38, $6a, $6c, $3a, $00
.db $00, $18, $18, $08, $00, $00, $00, $00
.db $00, $18, $30, $30, $30, $30, $18, $00
.db $00, $18, $0c, $0c, $0c, $0c, $18, $00
.db $00, $66, $3c, $7e, $7e, $3c, $66, $00
.db $00, $18, $18, $7e, $7e, $18, $18, $00
.db $00, $00, $00, $00, $18, $18, $10, $00
.db $00, $00, $00, $7e, $7e, $00, $00, $00
.db $00, $00, $00, $00, $00, $18, $18, $00
.db $00, $06, $0c, $18, $18, $30, $60, $00
numbers:
.db $00, $3c, $66, $66, $66, $66, $3c, $00
.db $00, $18, $78, $18, $18, $18, $7e, $00
.db $00, $3c, $66, $0c, $18, $30, $7e, $00
.db $00, $3c, $66, $1c, $06, $66, $3c, $00
.db $00, $66, $66, $7e, $06, $06, $06, $00
.db $00, $7e, $60, $7c, $06, $06, $7c, $00
.db $00, $3c, $60, $7c, $66, $66, $3c, $00
.db $00, $7e, $06, $06, $0c, $0c, $0c, $00
.db $00, $3c, $66, $3c, $7e, $66, $3c, $00
.db $00, $3c, $66, $66, $3e, $06, $3c, $00
.db $00, $18, $18, $00, $00, $18, $18, $00
.db $00, $18, $18, $00, $18, $18, $10, $00
.db $00, $06, $1e, $78, $78, $1e, $06, $00
.db $00, $7e, $7e, $00, $00, $7e, $7e, $00
.db $00, $60, $78, $1e, $1e, $78, $60, $00
.db $00, $3c, $66, $06, $1c, $00, $18, $00
.db $00, $3c, $66, $6e, $6a, $64, $30, $00
.db $00, $3c, $66, $7e, $66, $66, $66, $00
.db $00, $7c, $66, $7c, $66, $66, $7c, $00
.db $00, $3c, $66, $60, $60, $66, $3c, $00
.db $00, $7c, $66, $66, $66, $66, $7c, $00
.db $00, $7e, $60, $78, $60, $60, $7e, $00
.db $00, $7e, $60, $78, $60, $60, $60, $00
.db $00, $3c, $66, $60, $6e, $66, $3c, $00
.db $00, $66, $66, $7e, $66, $66, $66, $00
.db $00, $7e, $18, $18, $18, $18, $7e, $00
.db $00, $1e, $06, $06, $06, $66, $3c, $00
.db $00, $66, $6c, $78, $6c, $66, $66, $00
.db $00, $60, $60, $60, $60, $60, $7e, $00
.db $00, $66, $7e, $7e, $7e, $66, $66, $00
.db $00, $66, $76, $7e, $6e, $66, $66, $00
.db $00, $3c, $66, $66, $66, $66, $3c, $00
.db $00, $7c, $66, $7c, $60, $60, $60, $00
.db $00, $3c, $66, $66, $66, $7c, $3e, $00
.db $00, $7c, $66, $7c, $66, $66, $66, $00
.db $00, $3c, $60, $3c, $06, $66, $3c, $00
.db $00, $7e, $18, $18, $18, $18, $18, $00
.db $00, $66, $66, $66, $66, $66, $3c, $00
.db $00, $66, $66, $3c, $3c, $18, $18, $00
.db $00, $66, $66, $66, $7e, $7e, $66, $00
.db $00, $66, $3c, $18, $18, $3c, $66, $00
.db $00, $66, $66, $3c, $18, $18, $18, $00
.db $00, $7e, $06, $1c, $38, $60, $7e, $00
.db $00, $1c, $18, $18, $18, $18, $1c, $00
.db $00, $60, $30, $18, $18, $0c, $06, $00
.db $00, $38, $18, $18, $18, $18, $38, $00
.db $00, $18, $18, $3c, $3c, $66, $66, $00
.db $00, $00, $00, $00, $00, $00, $7e, $00
.db $00, $30, $18, $0c, $00, $00, $00, $00
.db $00, $00, $3c, $06, $3e, $66, $3e, $00
.db $00, $60, $60, $7c, $66, $66, $7c, $00
.db $00, $00, $3c, $66, $60, $66, $3c, $00
.db $00, $06, $06, $3e, $66, $66, $3e, $00
.db $00, $00, $3c, $66, $7e, $60, $3c, $00
.db $00, $0e, $18, $7e, $18, $18, $18, $00
.db $00, $3e, $66, $66, $3e, $06, $3c, $00
.db $00, $60, $60, $7c, $66, $66, $66, $00
.db $00, $18, $00, $18, $18, $18, $18, $00
.db $00, $18, $00, $18, $18, $18, $70, $00
.db $00, $60, $66, $6c, $78, $6c, $66, $00
.db $00, $18, $18, $18, $18, $18, $18, $00
.db $00, $00, $7c, $7e, $7e, $7e, $66, $00
.db $00, $00, $7c, $66, $66, $66, $66, $00
.db $00, $00, $3c, $66, $66, $66, $3c, $00
.db $00, $7c, $66, $66, $7c, $60, $60, $00
.db $00, $3e, $66, $66, $3e, $06, $06, $00
.db $00, $00, $7c, $66, $60, $60, $60, $00
.db $00, $00, $3c, $60, $3c, $06, $7c, $00
.db $00, $18, $18, $7e, $18, $18, $18, $00
.db $00, $00, $66, $66, $66, $66, $3e, $00
.db $00, $00, $66, $66, $66, $3c, $18, $00
.db $00, $00, $66, $7e, $7e, $7e, $3e, $00
.db $00, $00, $66, $3c, $18, $3c, $66, $00
.db $00, $00, $66, $66, $3e, $06, $3c, $00
.db $00, $00, $7e, $0c, $18, $30, $7e, $00
.db $00, $0e, $18, $70, $70, $18, $0e, $00
.db $00, $18, $18, $18, $18, $18, $18, $00
.db $00, $70, $18, $0e, $0e, $18, $70, $00
.db $00, $00, $30, $5a, $0c, $00, $00, $00
.db $00, $10, $20, $7e, $20, $10, $00, $00
fontDataEnd:

paletteData:
; If you swap which of the two below lines is commented, you get a "dark mode" of sorts
; (background color becomes black; grid is slightly lighter)
.dw $7fff, $1CE7, $3def, $0000	;null block
;.db $ff, $7f, $ef, $3d, $18, $63, $ff, $7f
.db $ff, $7f, $3f, $33, $9d, $02, $b3, $01
.db $ff, $7f, $9e, $25, $39, $1d, $53, $00
.db $ff, $7f, $8d, $7e, $e4, $7d, $40, $69
.db $ff, $7f, $ef, $7f, $c0, $7f, $e0, $62
.db $ff, $7f, $8c, $17, $08, $0b, $45, $02
.db $ff, $7f, $fb, $6d, $34, $51, $10, $40
.db $ff, $7f, $8d, $7d, $64, $74, $00, $60
.db $00, $00, $18, $63, $ef, $3d, $08, $21 ;monochrome

.db $00, $00, $08, $21, $8c, $31, $31, $46 ;darker null block (unused)
.db $31, $46, $b1, $19, $50, $01, $ca, $00
.db $31, $46, $d0, $10, $8d, $0c, $0a, $00
.db $31, $46, $46, $45, $e1, $44, $a0, $38
.db $31, $46, $08, $46, $00, $46, $80, $35
.db $31, $46, $e6, $09, $a4, $01, $22, $01
.db $31, $46, $ee, $38, $8a, $28, $08, $20
.db $31, $46, $c6, $44, $21, $40, $00, $30
.db $00, $00, $ef, $3d, $08, $21, $00, $00 ;dark mono
;.dw $00ff, $0ff0, $ff00, $f00f
;.dw $7fff, $1CE7, $3def, $0000
;.dw $7fff, $4f7d, $029d, $1d39
;.dw $7fff, $3a57, $1d39, $0000
;.dw $7fff, $7f21, $7de4, $4402 ;4
;.dw $7fff, $7fff, $7fc0, $7f21
;.dw $7fff, $5b83, $12c9, $3def
;.dw $7fff, $66fc, $5134, $0000
;.dw $7fff, $7eb9, $7464, $4402 ;8
;.dw $3def, $0c63, $1ce7, $0000
;.dw $3def, $25ae, $014e, $0c8c
;.dw $3def, $1d2b, $0c8c, $0000
;.dw $3def, $3d80, $3ce2, $2001 ;12
;.dw $3def, $3def, $3de0, $3d80
;.dw $3def, $2dc1, $0964, $1ce7
;.dw $3def, $316e, $288a, $0000
;.dw $3def, $3d4c, $3822, $2001 ;16

