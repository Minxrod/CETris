#include "includes\ti84pce.inc"

;program specfic constants:
LOCK_DISABLE 	equ	 -1
NULL_BLOCK 	equ	 0
vRamSplitSize 	equ	 (vRamEnd - vRam) / 2 
vRamSplit 	equ	 vRam + vRamSplitSize

;variables and memory regions
PSS 	equ	 plotSScreen
field 	equ	 PSS
fieldHeight 	equ	 25
fieldDrawHeight 	equ	 20
fieldDrawWidth 	equ	 10

blockDataSize 	equ	 8
blockGraphicSize 	equ	 2

;current status bits
csLockedBit 	equ	 0    ;locked block bit
csNewBlockBit 	equ	 1  ;prev. frame requests new block
csClearLineBit 	equ	 2 ;if block cleared a line
csGarbageBit 	equ	 3   ;apply garbage stored (unused)
csRotateTBit 	equ	 4   ;T-block last move was rotate
csWallKicked 	equ	 5   ;if last rotate was kicked
csUsedHold 	equ	 6     ;hold was used once already

;mino type data (held piece, next pieces, etc.)
minoTypes	equ	PSS + 600
holdTOfs	equ	0
bag1Ofs		equ	1
bag2Ofs		equ	8
demoTOfs	equ	15

holdT		equ	minoTypes + holdTOfs
nextBag1	equ 	minoTypes + bag1Ofs
nextBag2	equ	minoTypes + bad2Ofs
demoT		equ	minoTypes + demoTOfs

;current mino data
curData 	equ	 PSS + 256
curX 	equ	 curData + 0
curY 	equ	 curData + 1
curR 	equ	 curData + 2
curT 	equ	 curData + 3
curStatus 	equ	 curData + 4
curBlock 	equ	 curData + 5 ;size: 10
curDataSize 	equ	 curBlock + blockDataSize + blockGraphicSize - curX

curXOfs	equ	0
curYOfs	equ	1
curStatusOfs	equ	4
curBlockOfs	equ	5

;inbetween cur and old is a middle data chunk
;used to handle intermediate frames due to
;double-buffer
midOfs 	equ	 curDataSize
midData 	equ	 midOfs + curData

;saves data from previous frame here
oldOfs 	equ	 2 * midOfs ;alt. 2 * curDataSize
oldData 	equ	 oldOfs + curData

;temp data to calculate ghost piece
tempOfs	equ	 3 * curDataSize
tempData	equ tempOfs + curData

midTempOfs	equ	4 * curDataSize
midTempData	equ	curData + midTempOfs

;2-previous frame's ghost
oldTempOfs equ	 5 * curDataSize
oldTempData	equ	curData + oldTempOfs

oldX 	equ	 oldOfs + curX
oldY 	equ	 oldOfs + curY
oldR 	equ	 oldOfs + curR
oldT 	equ	 oldOfs + curT
oldBlock 	equ	 oldOfs + curBlock

gameVars	equ	PSS + 360
score 	equ	 gameVars
level 	equ	 gameVars + 4
lines 	equ	 gameVars + 8
linesToNextLevel 	equ	 gameVars + 12
timerT 	equ	 gameVars + 16
lockTimer 	equ	 gameVars + 20	;lock delay
globalTimer 	equ	 gameVars + 24
spawnTimer	equ	 gameVars + 28	;ARE
clearTimer	equ	 gameVars + 32	;line clear timer
highscore 	equ	 gameVars + 40
garbageTimer 	equ	 gameVars + 44
queuedGarbage 	equ	 gameVars + 48
linesClear 	equ	 gameVars + 52 ;only needs one byte
lineClearInfo 	equ	 gameVars + 56 ;only needs one byte
lcBackToBack 	equ	 0
lcPreviousB2B 	equ	 1
lcTSpin 	equ	 2 ;set even if no lines clear
queuedAction	equ	 gameVars + 60 ;IHS, IRS
qaHold	equ	 0
qaRotateLeft	equ	 1
qaRotateRight	equ	 2
lockResets	equ	 gameVars + 64 ; move reset related

miscVars	equ	 gameVars + 68

;not the best location
;but I don't really know where to put this stuff
saveDataPTR 	equ	 miscVars
menuSelection 	equ	 miscVars + 4
numberSelection 	equ	 miscVars + 8

ruleVars = miscVars + 100 + 4 ;because rules uses -4 offset for some reason ;520ish

;data for various rules, stored as offset from rules
;garbage set
rMode 	equ	 -4 ;why did I forget this
rRGT 	equ	 -1 ;rising garbage timer
rRGD 	equ	 -2;rising garbage density
;generated set
rGGA 	equ	 -1;generated garbage amt
rGGD 	equ	 -2 ;generated garbage density
;line clear data
rLCW 	equ	 -3;lines needed to clear to win

rules 	equ	 ruleVars

rfBasic 	equ	 0 ;basic mechanics
rfExtra 	equ	 1 ;extra mechanics
rfWin 	equ	 2 ;win condition currently set
rfScore 	equ	 3 ;high score method

rbitGameCont 	equ	 0
rbitGameWon 	equ	 1
rbitSRSEnabled 	equ	 2
rbitPreviewEnabled 	equ	 3
rbitHoldEnabled 	equ	 4
rbitHardDropEnabled 	equ	 5
rbitBagEnabled 	equ	 6
rbitGhostEnabled	equ	 7

;rbitUnusedExtra 	equ	 0
rbitClearAnimation	equ	 0
rbitGarbageRising 	equ	 1
rbitGarbageInitial 	equ	 2
rbitCascadeGravity 	equ	 3
rbitCascadeAnimation 	equ	 4
rbitLockAnimation	equ	 5
rbitInstantHold		equ	 6
rbitInstantRotate	equ	 7


rbitLinesClear 	equ	 0
rbitRow0Clear 	equ	 1 ;for garbage/generated games
rbitCountdown 	equ	 2 ;timed survival games
rbitGarbageClear	equ	 3

rbitHighScore 	equ	 0
rbitLowTime 	equ	 1
rbitHighLine 	equ	 2

rNull 	equ	 0
rGame 	equ	 1 << rbitGameCont
rGameWon 	equ	 1 << rbitGameWon ;if game was won or lost
rSRS 	equ	 1 << rbitSRSEnabled
rPreview 	equ	 1 << rbitPreviewEnabled
rHold 	equ	 1 << rbitHoldEnabled 
rHardDrop 	equ	 1 << rbitHardDropEnabled
rBag 	equ	 1 << rbitBagEnabled
rGhost	equ	 1 << rbitGhostEnabled

;rUnused 	equ	 1 << rbitunusedextra
rClearAnim	equ	 1 << rbitClearAnimation
rRising 	equ	 1 << rbitGarbageRising
rGenerated 	equ	 1 << rbitGarbageInitial
rCascade 	equ	 1 << rbitCascadeGravity
rCascadeAnim 	equ	 1 << rbitCascadeAnimation
rLockAnim	equ	 1 << rbitLockAnimation
rIHS		equ	 1 << rbitInstantHold
rIRS		equ	 1 << rbitInstantRotate

rLines 	equ	 1 << rbitLinesClear
rRow0 	equ	 1 << rbitRow0Clear
rCountdown 	equ	 1 << rbitCountdown
rGarbage	equ	 1 << rbitGarbageClear

rScore 	equ	 1 << rbitHighScore
rTime 	equ	 1 << rbitLowTime
rLine 	equ	 1 << rbitHighLine 

rMarathon 	equ	 rGame | rSRS | rPreview | rHold | rHardDrop | rBag | rGhost
rRetro 	equ	 rGame

rCascadeExtra 	equ	 rCascade | rCascadeAnim

lockDelay 	equ	 PSS + 760
theme 	equ	 PSS + 764 ;precedes blockData
spawnDelay	equ	 PSS + 768
clearDelay	equ	 PSS + 772
blockData 	equ	 PSS + 776
buttonData 	equ	 PSS + 1024
blockGraphic 	equ	 PSS + 1280

menuJumpTable 	equ	 PSS + 2048
jptMainMenu 	equ	 menuJumpTable + 0
jptInitGame 	equ	 menuJumpTable + 4	
jptExit 	equ	 menuJumpTable + 8
jptActiveMenu 	equ	 menuJumpTable + 12
jptGetStringInList 	equ	 menuJumpTable + 16
jptGetStringPTRSelection 	equ	 menuJumpTable + 20
jptSetNumber 	equ	 menuJumpTable + 24
jptDrawObject 	equ	 menuJumpTable + 28 ;for custom draws
jptCheckKey 	equ	 menuJumpTable + 32 ;yes
jptSwapVRamPTR 	equ	 menuJumpTable + 36 ;okay
jptDrawMino	equ	 menuJumpTable + 40 ;eh, maybe these should be split up somehow?

userVars 	equ	 PSS + 3072

;key ids for important operations
;button repeat info etc.
keyIDs:
buttonID	equ	0
buttonTimeStart	equ	1
buttonTimeRepeat	equ	2
buttonTimer	equ	3
buttonDataSize	equ	4

buttonLeft 	equ	 0 * buttonDataSize + buttonData
buttonRight 	equ	 1 * buttonDataSize + buttonData
buttonSoft 	equ	 2 * buttonDataSize + buttonData
buttonHard 	equ	 3 * buttonDataSize + buttonData
buttonRotateLeft 	equ	 4 * buttonDataSize + buttonData
buttonRotateRight 	equ	 5 * buttonDataSize + buttonData
buttonHold 	equ	 6 * buttonDataSize + buttonData
buttonPause 	equ	 7 * buttonDataSize + buttonData

mbuttonUp 	equ	 8 * buttonDataSize + buttonData
mbuttonDown 	equ	 9 * buttonDataSize + buttonData
mbuttonLeft 	equ	 10 * buttonDataSize + buttonData
mbuttonRight 	equ	 11 * buttonDataSize + buttonData

mbuttonConfirm 	equ	 12 * buttonDataSize + buttonData
mbuttonBack 	equ	 13 * buttonDataSize + buttonData
mbuttonQuit 	equ	 14 * buttonDataSize + buttonData

noRepeat 	equ	 -1

;graphical and data resources
;note: info must be RELOCATED to this location.
dataLocation 	equ	 saveSScreen

;save file consts
savKeys 	equ	 0
savTheme 	equ	 64 ;appvar name for theme
savThemeSub 	equ	 73 ;block theme id, etc.
savHighscore 	equ	 74
savHighSize 	equ	 8
savSize 	equ	 512 ;save should not pass this size, kinda large.
savScoreSize 	equ	 savSize - savHighScore ;

;drawing stuff
drawMinoErase 	equ	 0
drawMinoHalf 	equ	 1
drawMinoDark 	equ	 2
;drawMinoFromPTR 	equ	 3 ;no longer useful

;bit set/reset when an object must be updated
redrawObjBit 	equ	 7

;variou	equates for itemsInfo
iDataType 		equ	 0 ;type of data (use type~ to check)
iDataXL			equ	 1 ;x low byte
iDataXH			equ	 2 ;x high byte
iDataY			equ	 3 ;y
iDataA			equ	 4 ;a (used as color/palette for most)
iDataPTR		equ	 5 ;ptr offset
iDataPTRL		equ	 5 ;low byte
iDataPTRH		equ	 6 ;high byte
iDataPTRU		equ	 7 ;upper byte?
iDataW			equ	 8 ;extra data (width, # digits)
iDataH			equ	 9 ;extra data 2 (height)
iDataSize		equ	 10 ;size of data struct

iExtType		equ	 10 ;
iExtTile		equ	 11 ;ptr to tile sprite data
iExtTileset		equ	 14 ;ptr to sprite/palette pairs
iExtMap			equ	 17 ;ptr to map data
iExtSize		equ	 20 ;size of extended data


;object types, almost entirely used in tetrice_dat
typeSprite1bpp	equ	0
typeSprite2bpp	equ	1
typeSprite4bpp	equ	2
typeSprite8bpp	equ	3
typeSprite1Half	equ	4
typeSprite2Half	equ	5
typeSprite4Half	equ	6
typeSprite8Half	equ	7
typeString	equ	8
typeNumber	equ	9
typeMenu	equ	10
typeNumber8	equ	11
typeMap	equ	12
typeBox	equ	13
typeCustom	equ	14 ;jumps to PTR provided, enables custom draw code
typeCompound	equ	15 ;uses pointer to another set of objects
typeExtended	equ 	16 ;ignore
typeIgnore	equ	16 ;same as extended, use for clarity of purpose (ex. disable an object for some reason)

;data references used in main program
;all defined relative to SSS, so doesn't need actual data.
drefSize 	equ	 3
refSize 	equ	 drefSize

drefSprite 	equ	 drefSize * 0 + dataLocation
drefFont 	equ	 drefSize * 1 + dataLocation
drefBlocks 	equ	 drefSize * 2 + dataLocation
drefBackground 	equ	 drefSize * 3 + dataLocation
drefPalette		equ	 drefSize * 4 + dataLocation
drefMenu 	equ	 drefSize * 5 + dataLocation
drefPause 	equ	 drefSize * 6 + dataLocation
drefIInfo 	equ	 drefSize * 7 + dataLocation
drefGameOver 	equ	 drefSize * 8 + dataLocation
refField 	equ	 refSize * 9 + dataLocation
refHold 	equ	 refSize * 10 + dataLocation
refLevel 	equ	 refSize * 11 + dataLocation
refScore 	equ	 refSize * 12 + dataLocation
refLines 	equ	 refSize * 13 + dataLocation
refPreview 	equ	 refSize * 14 + dataLocation
refTimer 	equ	 refSize * 15 + dataLocation
;this is where the appvar/graphical data can set itself up, if necessary
;example: loading themes
initDataStart 	equ	 refSize * 16 + dataLocation

sp1bpp 	equ	 0
sp2bpp 	equ	 1
sp4bpp 	equ	 2
sp8bpp 	equ	 3
spHalf	equ	 4 ;OR with previous ~ default is 1bpp
