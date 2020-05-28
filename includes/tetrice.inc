#include "includes\ti84pce.inc"

;program specfic constants:
LOCK_DISABLE = -1
NULL_BLOCK = 8
vRamSplitSize = (vRamEnd - vRam) / 2 
vRamSplit = vRam + vRamSplitSize

;variables and memory regions
PSS = plotSScreen
field = PSS
fieldHeight = 25
fieldDrawHeight = 20
fieldDrawWidth = 10

blockDataSize = 8
blockGraphicSize = 2

;current status bits
csLockedBit = 0    ;locked block bit
csNewBlockBit = 1  ;prev. frame requests new block
csClearLineBit = 2 ;if block cleared a line
csGarbageBit = 3   ;apply garbage stored (unused)
csRotateTBit = 4   ;T-block last move was rotate
csWallKicked = 5   ;if last rotate was kicked
csUsedHold = 6     ;hold was used once already

holdT= PSS + 256
;current mino data
curData = PSS + 266
curX = curData + 0
curY = curData + 1
curR = curData + 2
curT = curData + 3
curStatus = curData + 4
curBlock = curData + 5 ;size: 10
curDataSize = curBlock + blockDataSize + blockGraphicSize - curX

;inbetween cur and old is a middle data chunk
;used to handle intermediate frames due to
;double-buffer
;note: should not actually be used, probably?
midOfs = curDataSize
midData = midOfs + curData

;saves data from previous frame here
oldOfs = 2 * midOfs ;alt. 2 * curDataSize
oldData = oldOfs + curData

oldX = oldOfs + curX
oldY = oldOfs + curY
oldR = oldOfs + curR
oldT = oldOfs + curT
oldBlock = oldOfs + curBlock

score = PSS + 320
level = PSS + 324
lines = PSS + 328 
linesToNextLevel = PSS + 332
timerT = PSS + 336
lockTimer = PSS + 340
globalTimer = PSS + 348
highscore = PSS + 352
garbageTimer = PSS + 356
queuedGarbage = PSS + 360
linesClear = PSS + 364
lineClearInfo = PSS + 365
lcBackToBack = 0
lcPreviousB2B = 1
lcTSpin = 2 ;set even if no lines clear

;not the best location
;but I don't really know where to put this stuff
saveDataPTR = PSS + 400 
menuSelection = PSS + 404
numberSelection = PSS + 408

;data for various rules, stored as offset from rules
;garbage set
rMode = -4 ;why did I forget this
rRGT = -1 ;rising garbage timer
rRGD = -2;rising garbage density
;generated set
rGGA = -1;generated garbage amt
rGGD = -2 ;generated garbage density
;line clear data
rLCW = -3;lines needed to clear to win

rules = PSS + 512

rfBasic = 0 ;basic mechanics
rfExtra = 1 ;extra mechanics
rfWin = 2 ;win condition currently set
rfScore = 3 ;high score method

rbitGameCont = 0
rbitGameWon = 1
rbitSRSEnabled = 2
rbitPreviewEnabled = 3
rbitHoldEnabled = 4
rbitHardDropEnabled = 5
rbitBagEnabled = 6

rbitunusedextra = 0
rbitGarbageRising = 1
rbitGarbageInitial = 2
rbitCascadeGravity = 3
rbitCascadeAnimation = 4

rbitLinesClear = 0
rbitRow0Clear = 1 ;for garbage/generated games
rbitCountdown = 2 ;timed survival games

rbitHighScore = 0
rbitLowTime = 1
rbitHighLine = 2

rNull = 0
rGame = 1 << rbitGameCont
rGameWon = 1 << rbitGameWon ;if game was won or lost
rSRS = 1 << rbitSRSEnabled
rPreview = 1 << rbitPreviewEnabled
rHold = 1 << rbitHoldEnabled 
rHardDrop = 1 << rbitHardDropEnabled
rBag = 1 << rbitBagEnabled

rUnused = 1 << rbitunusedextra
rRising = 1 << rbitGarbageRising
rGenerated = 1 << rbitGarbageInitial
rCascade = 1 << rbitCascadeGravity
rCascadeAnim = 1 << rbitCascadeAnimation

rLines = 1 << rbitLinesClear
rRow0 = 1 << rbitRow0Clear
rCountdown = 1 << rbitCountdown

rScore = 1 << rbitHighScore
rTime = 1 << rbitLowTime
rLine = 1 << rbitHighLine 

rMarathon = rGame | rSRS | rPreview | rHold | rHardDrop | rBag
rRetro = rGame

rCascadeExtra = rCascade | rCascadeAnim

lockDelay = PSS + 760
theme = PSS + 764 ;precedes blockData
blockData = PSS + 768
buttonData = PSS + 1024
blockGraphic = PSS + 1280

menuJumpTable = PSS + 2048
jptMainMenu = menuJumpTable + 0
jptInitGame = menuJumpTable + 4	
jptExit = menuJumpTable + 8
jptActiveMenu = menuJumpTable + 12
jptGetStringInList = menuJumpTable + 16
jptGetStringPTRSelection = menuJumpTable + 20
jptSetNumber = menuJumpTable + 24
jptDrawObject = menuJumpTable + 28 ;for custom draws
jptCheckKey = menuJumpTable + 32 ;yes
jptSwapVRamPTR = menuJumpTable + 36 ;okay

;key ids for important operations
;button repeat info etc.
keyIDs:
buttonID=0
buttonTimeStart=1
buttonTimeRepeat=2
buttonTimer=3
buttonDataSize=4

buttonLeft = 0 * buttonDataSize + buttonData
buttonRight = 1 * buttonDataSize + buttonData
buttonSoft = 2 * buttonDataSize + buttonData
buttonHard = 3 * buttonDataSize + buttonData
buttonRotateLeft = 4 * buttonDataSize + buttonData
buttonRotateRight = 5 * buttonDataSize + buttonData
buttonHold = 6 * buttonDataSize + buttonData
buttonPause = 7 * buttonDataSize + buttonData

mbuttonUp = 8 * buttonDataSize + buttonData
mbuttonDown = 9 * buttonDataSize + buttonData
mbuttonLeft = 10 * buttonDataSize + buttonData
mbuttonRight = 11 * buttonDataSize + buttonData

mbuttonConfirm = 12 * buttonDataSize + buttonData
mbuttonBack = 13 * buttonDataSize + buttonData
mbuttonQuit = 14 * buttonDataSize + buttonData

noRepeat = -1

;graphical and data resources
;note: info must be RELOCATED to this location.
SSS = saveSScreen

;save file consts
savKeys = 0
savTheme = 64 ;appvar name for theme
savThemeSub = 73 ;block theme id, etc.
savHighscore = 74
savHighSize = 8
savSize = 512 ;save should not pass this size, kinda large.
savScoreSize = savSize - savHighScore ;

;drawing stuff
drawMinoErase = 0
drawMinoHalf = 1
drawMinoDark = 2
;drawMinoFromPTR = 3 ;no longer useful

;bit set/reset when an object must be updated
redrawObjBit = 7

;various equates for itemsInfo
iDataType 	= 0 ;type of data (use type~ to check)
iDataXL		= 1 ;x low byte
iDataXH		= 2 ;x high byte
iDataY		= 3 ;y
iDataA		= 4 ;a (used as color/palette for most)
iDataPTR	= 5 ;ptr offset
iDataPTRL	= 5 ;low byte
iDataPTRH	= 6 ;high byte
iDataPTRU	= 7 ;upper byte?
iDataW		= 8 ;extra data (width, # digits)
iDataH		= 9 ;extra data 2 (height)
iDataSize	= 10 ;size of data struct

;object types, almost entirely used in tetrice_dat
typeTetris=0
typeString=1
typeNumber=2
typeSprite=3 ;obsolete, acts like typeSprite8bpp
typeHold=4
typePreview=5
typeBox=6
typeMenu=7
typeSprite8bpp=8
typeSprite4bpp=9
typeSprite2bpp=10
typeSprite1bpp=11
typeNumber8=12
typeList=13 ;unimplemented, do not use
typeCustom=14 ;jumps to PTR provided, enables custom draw code
typeMap=15

;data references used in main program
;all defined relative to SSS, so doesn't need actual data.
drefSize = 3
refSize = drefSize

drefSprite = drefSize * 0 + SSS
drefFont = drefSize * 1 + SSS
drefBlocks = drefSize * 2 + SSS
drefBackground = drefSize * 3 + SSS
drefPalette	= drefSize * 4 + SSS
drefMenu = drefSize * 5 + SSS
drefPause = drefSize * 6 + SSS
drefIInfo = drefSize * 7 + SSS
drefGameOver = drefSize * 8 + SSS
refField = refSize * 9 + SSS
refHold = refSize * 10 + SSS
refLevel = refSize * 11 + SSS
refScore = refSize * 12 + SSS
refLines = refSize * 13 + SSS
refPreview = refSize * 14 + SSS
refTimer = refSize * 15 + SSS
;this is where the appvar/graphical data can set itself up, if necessary
;example: loading themes
initDataStart = refSize * 16 + SSS

sp1bpp = 0
sp2bpp = 1
sp4bpp = 2
sp8bpp = 3
 
