#include "includes\ti84pce.inc"

 .assume ADL=1
 .org userMem-2
 .db tExtTok,tAsm84CeCmp

;program specfic constants:
LOCK_DISABLE = -1
NULL_BLOCK = -1
vRamSplitSize = (vRamEnd - vRam) / 2 
vRamSplit = vRam + vRamSplitSize

;variables and memory regions
PSS = plotSScreen
field = PSS

blockDataSize = 8
blockGraphicSize = 2

;current status bits
csLockedBit = 0    ;locked block bit
csNewBlockBit = 1  ;prev. frame requests new block
csClearLineBit = 2 ;if block cleared a line
csGarbageBit = 3   ;apply garbage stored

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
rbitGameEndless = 1
rbitSRSEnabled = 2
rbitPreviewEnabled = 3
rbitHoldEnabled = 4
rbitHardDropEnabled = 5

rbitCascadeGravity = 0
rbitGarbageRising = 1
rbitGarbageInitial = 2

rbitLinesClear = 0
rbitRow0Clear = 1 ;for garbage/generated games
rbitCountdown = 2 ;timed survival games

rbitHighScore = 0
rbitLowTime = 1
rbitHighLine = 2

rNull = 0
rGame = 1 << rbitGameCont
rEndless = 1 << rbitGameEndless ;unused in favor of clearing win condition check flags
rSRS = 1 << rbitSRSEnabled
rPreview = 1 << rbitPreviewEnabled
rHold = 1 << rbitHoldEnabled 
rHardDrop = 1 << rbitHardDropEnabled

rCascade = 1 << rbitCascadeGravity
rRising = 1 << rbitGarbageRising
rGenerated = 1 << rbitGarbageInitial

rLines = 1 << rbitLinesClear
rRow0 = 1 << rbitRow0Clear
rCountdown = 1 << rbitCountdown

rScore = 1 << rbitHighScore
rTime = 1 << rbitLowTime
rLine = 1 << rbitHighLine 

rMarathon = rGame | rSRS | rPreview | rHold | rHardDrop
rRetro = rGame

lockDelay = PSS + 760
theme = PSS + 764 ;precedes blockData
blockData = PSS + 768
buttonData = PSS + 1024
blockGraphic = PSS + 1280
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

;bit set/reset when an object must be updated
redrawObjBit = 7

;main program
main:
 ld (preserveSP),sp
 call initLCD
 
 call loadSaveVar ;get ptr to save data from appvar
 inc de
 inc de ;past size of appvar
 ld (saveDataPTR),de
 call loadSave ;actually read save data
 
 call initData
 call initKeyTimer
 
 call defaultInfo
 
 
 jp mainMenu
 
exit:
 call saveSave
 call resetLCD
 call restoreKeyboard
 
 ld sp,(preserveSP)
 ret

preserveSP:
.dl 0
saveDataPTR:
.dl 0

defaultInfo:
 ld ix,rules
 ld (ix+rfBasic), rMarathon
 ld (ix+rfExtra), rNull ;no extra rules
 ld (ix+rfWin), rLines
 
 xor a
 ld (ix+rRGT),a
 ld (ix+rRGD),a
 ld (ix+rLCW),150
 
 inc a ;a=1
 ld (level),a ;starting level is by default one
 ret 

mainMenu:
 call defaultInfo ;ensure start menu is accurate
 ld ix,(drefMenu)
 jp activeMenu
 
initGame:
 ld a,0
 ld (lines),a

 ld hl,0
 ld (score),hl ;don't keep old score/time!
 ld (globaltimer),hl
 
 ld a,NULL_BLOCK
 ld (holdT),a
 
 call initBag ;also initializes random I guess?
 call initField ;depends on RNG being seeded by initbag, which init's rng system
 
 call newBlock
 
 ;first draw to set up screen
 call clearLCD
 ld ix,(drefIInfo)
 call resetAllDrawCheck
 ld ix,(drefIInfo)
 call drawObjectsNoReset
 call swapVRamPTR
 ld ix,(drefIInfo)
 call drawObjects
 call swapVRamPTR
 
game:
 call scanKeys
 ld ix,mbuttonQuit
 call checkKeyDown
 jp c, exit ;exit if pressed
 
 call shiftOldData
 ;part of update code, essentially
 ;might move to update sometime
 
 call userUpdate
 call update
 call drawGame
 
 ld hl,(globalTimer)
 inc hl
 ld (globalTimer),hl
 
 ld hl, (refTimer)
 res redrawObjBit, (hl)
 
 ld ix,rules
 bit rBitGameCont, (ix+rfBasic)
 jr nz, game ;jump if nz: bit is 1, game is going
 jp gameEndInit

initField:
 ld hl,field
 ld de,field+1
 ld bc,255
 ld (hl),NULL_BLOCK
 ldir
 
 ld hl, rules+rfExtra
 bit rbitGarbageInitial, (hl)
 jr nz, generateGarbage
 ret
 
generateGarbage:
 ld ix, rules
 ld a,20
 sub (ix+rGGA) ;garbage rows to generate
 ld d,a
 add a,a
 add a,a
 add a,d
 add a,a ;10a = ofs from field
 ld de,0
 ld e,a
 ld hl,field
 add hl,de
 
 ld b,(ix+rGGA)
genGarbageLoop:
 push bc
 push hl ;save field position
 
 ld b,(ix+rGGD)
 ld ix, rules
 call generateGarbageRow
 pop hl
 ld de,10
 add hl,de
 pop bc
 djnz genGarbageLoop
 ret
 
;hl=ptr to field row
;b=density to generate
generateGarbageRow:
 push hl
 push bc
 push hl
 call rand
 push de
 pop hl
 ;hl = random
 ld a,10
 call _DivHLByA
 ;a = [0,9] random
 pop hl ;get fieldptr
 ld de,0
 ld e,a
 ;hl = fieldptr de = ofs
backAgainGenerate: ;try again with new ofs here
 add hl,de ;fieldptr+ofs
 ld a,(hl)
 or a,a ;clear carry
 sbc hl,de ;fieldptr +ofs -ofs
 cp NULL_BLOCK ;block is empty
 jr nz, tryAgainGenerate ;isn't an empty block, try again
 ld a,7
 add hl,de
 ld (hl),a ;counting loop
 pop bc
 pop hl ;orig. fieldptr
 djnz generateGarbageRow
 ret

tryAgainGenerate:
 ;hl = fieldptr
 ;de = ofs
 ld a,e
 inc a
 cp 10
 jr nz, skipResetA0
 xor a,a
skipResetA0:
 ld e,a 
 jr backAgainGenerate
 
shiftOldData:
 ;copy old mino data
 ;to use for clearing old draws
 ld hl, midData
 ld de, oldData
 ld bc, curDataSize
 ldir
 
 ld hl, curData
 ld de, midData
 ld bc, curDataSize
 ldir
 ret
 
userUpdate:
 ld ix,buttonLeft
 call checkKeyDown
 call c, checkMoveLeft
 
 ld ix,buttonRight
 call checkKeyDown
 call c, checkMoveRight
 
 ld ix,buttonRotateLeft
 call checkKeyDown
 call c, checkRotationLeft
 
 ld ix,buttonRotateRight
 call checkKeyDown
 call c, checkRotationRight
 
 ld ix,buttonHold
 call checkKeyDown
 call c, hold
 
 ld ix,buttonPause
 call checkKeyDown
 call c, pause
 ret
 
pause:
 ld ix,buttonPause
 call waitNoButton
 
pauseLoop:
 call scanKeys

 ld ix,buttonPause
 call checkKeyDown
 jr c, pauseEnd
 ld ix,mbuttonQuit
 call checkKeyDown
 jr c, pauseEnd
 
 ld ix, (drefPause)
 call drawObjectsNoReset
 call swapVRamPTR
 jr pauseLoop

pauseEnd:
 call scanKeys
 
 ld ix,buttonPause
 call checkKeyDown
 jr c, pauseEnd ;wait until release of mode

 ld ix, (drefIInfo)
 call resetAllDrawCheck
 ld ix, (drefIInfo)
 call drawObjectsNoReset
 call swapVRamPTR
 ld ix, (drefIInfo)
 call drawObjects
 call swapVRamPTR
 ret
 
hold:
 ld ix,rules
 bit rbitHoldEnabled,(ix+0)
 ret z ;hold is disabled

 ;tell draw system to redraw HOLD
 ld ix,(refHold)
 res redrawObjBit, (ix+iDataType)
 
 ld a,(holdT)
 cp NULL_BLOCK
 jr z, firstHold
 
 ;need to swap held block and current block using transfer memory
 ld a,(holdT)
 ld b,a
 ld a,(curT)
 ld c,a
 ld a,b
 ld (curT),a
 ld a,c
 ld (holdT),a
 
 ld a,(curT)
 call determinedBlock
 ret
firstHold:
 ld a,(curT)
 ld (holdT),a
 
 call newBlock ;geneate a new block: nothing to load
 ret
 
dropMultiple:
 ld a,(level)
 sub 9
 ld b,a
dropAllTimes:
 push bc
 call checkBlockDown
 pop bc
 djnz dropAllTimes
 jr dropReturn
 
update:
 ld hl, curStatus + midOfs ;last frame status
 bit csNewBlockBit, (hl) ;request new block
 call nz, newBlock ;if set, create block
 
 ld hl, rules+rfWin
 bit rbitLinesClear, (hl)
 jr z, skipEndCheck
 ;check if lines cleared is less than lines needed
 ld a,(lines)
 ld hl, rules+rLCW
 cp (hl) ;nc (hl)<=a ;c (hl)>a
 jr c, skipEndCheck
 ;game has ended, player won
 ld hl, rules
 res rbitGameCont, (hl)
 
skipEndCheck:
 
 ld hl,(timerT)
 inc hl
 ld (timerT),hl
 
 ;calculate gravity
 ld b,l ;current time
 ld a, (level)
 add a,a
 ld l, a ;l=level
 ld a, 60
 sub l ;60 - level
 jr z, dropMultiple
 jr c, dropMultiple
 cp b
 jr c, drop

 ld ix,buttonSoft
 call checkKeyDown
 jr c, userdrop
 ld ix,buttonHard
 call checkKeyDown
 jr c, harddrop
dropReturn:
 
 ;check if lock
 ld a,(lockTimer)
 cp LOCK_DISABLE
 jr z, skipLockCheck
 dec a
 ld (lockTimer),a
 cp 0
 jr z, lock
skipLockCheck:
 ret
hardDrop:
 ld ix,rules
 bit rbitHardDropEnabled,(ix+0)
 ret z ;hard drop is disabled

hardDropLoop:
 ld a,(lockTimer)
 cp LOCK_DISABLE
 jr nz, hdrop ;lock not disabled
 ld hl,(score)
 inc hl
 inc hl
 ld (score),hl
 ld hl,0
 call checkBlockDown
 jr hardDropLoop
hdrop:
 ld a,1
 ld (lockTimer),a
 ld ix,(refScore)
 res redrawObjBit, (ix+iDataType)
 jr dropReturn
 ret
 
userDrop:
 ld a,(lockTimer)
 cp LOCK_DISABLE
 jr nz, drop
 ld hl,(score)
 inc hl
 ld (score),hl
 ld ix,(refScore)
 res redrawObjBit, (ix+iDataType)
 
drop:
 ld hl,0
 ld (timerT),hl
 call checkBlockDown
 jr dropReturn
 
lock:
 ld a,LOCK_DISABLE
 ld (lockTimer),a
 ld hl, curBlock
 ld b,4
lockAllBlocks:
 push bc
 push hl ;save the block data ptr
 ld c,(hl)
 inc hl
 ld b,(hl) ;(c,b) = (ofsx, ofsy)
 ld a,(curX)
 add a,c
 ld h,a
 ld a,(curY)
 add a,b
 ld l,a
 call checkBlock ;get offset from coords
 ld a,(curT)
 ld (hl),a ;save block type
 pop hl
 inc hl
 inc hl
 pop bc
 djnz lockAllBlocks
 
 ld hl, curStatus
 set csLockedBit,(hl) ;this is the lock frame.
 ;since it locks here, do not clear block when drawn.
 call checkLines ;only check line clears after a lock
 
 ;call newBlock
 ld hl, curStatus
 set csNewBlockBit,(hl) ;new block should be created
 ;
 
 ret
gameEnd:
 ld ix,rules
 res rBitGameCont, (ix+0) ;game is not going
 ret
 
checkLines:
 ld a,0
 ld (linesClear),a
 ld b,20
 ld hl, field
 
checkAllLines:
 push bc
 ld e, 0
 ld b,10
checkOneLine:
 push bc
 ld a,NULL_BLOCK
 cp (hl)
 jr z,noIncrementE
 inc e
noIncrementE:
 inc hl
 pop bc
 djnz checkOneLine
 ld a,10
 pop bc
 push bc ;restore "y value" of row being checked
 cp e
 call z, clearLine
 pop bc
 djnz checkAllLines
 
 ;add to lines cleared
 ld a,(linesClear)
 cp 0
 jr z, noLinesClear
 ;must redraw field, update scores, etc.
 ld ix, (refField)
 res redrawObjBit, (ix+iDataType)
 
 ld ix, (refLevel)
 res redrawObjBit, (ix+iDataType)
 
 ld ix, (refScore)
 res redrawObjBit, (ix+iDataType)
 
 ld ix, (refLines)
 res redrawObjBit, (ix+iDataType)
 
 ld hl, curStatus
 set csClearLineBit, (hl) ;clear bit 
 
 ld de,0
 ld e,a ;de is lines cleared
 ld hl,(lines)
 add hl,de
 ld (lines),hl
 
 ld a,(linesToNextLevel)
 add a,e ;e=lines cleared
 ld (linesToNextLevel),a
 
 ld hl,pointsPerLine
 add hl,de
 ld e,(hl)
 ld a,(level) ;level multiplier
 ld d,a
 mlt de ;level * linesClear
 
 ld hl,0 ;de is score to multiply by 100
 add hl,de ;1
 add hl,hl ;2
 add hl,hl ;4
 add hl,de ;5
 add hl,hl ;10
 push hl
 pop de
 add hl,hl ;20
 add hl,hl ;40
 add hl,de ;50
 add hl,hl ;100*score
 
 ld de,(score)
 add hl,de ;score+=100a
 ld (score),hl
 
 ;increment level for every 10 lines cleared
 ld a,(linesToNextLevel)
 sub 10 ;equiv flags to cp 10 and saves sub after
 jr c, noLevelUp
 ld (linesToNextLevel),a
 ld hl, (level)
 inc hl
 ld (level),hl
noLevelUp:
noLinesClear:
;all of the above do not update when not cleared.
 ret
 
clearLine:
 ld a,b
 cp 1 ;last row is being check and is clear
 jr nz, skipGameEndR0 ;not last row
 ld ix,rules
 bit rbitRow0Clear, (ix+rfWin)
 jr z, skipGameEndR0 ;if row 0 is not the win condition, don't end game
 res rbitGameCont, (ix+rfBasic)
 
skipGameEndR0: 
 push hl
 ld bc,field
 ld de,11
 or a ;clear carry
 sbc hl,bc ;field + displacement - field
 or a
 sbc hl,de ;displacement - 10
 push hl
 pop bc
 ld hl, field
 add hl,bc
 ex de,hl
 ld hl, field + 10
 add hl,bc
 ex de,hl
 lddr ;copy rows down 10
 
 ld hl, field
 ld de, field+1
 ld bc,9
 ld (hl),NULL_BLOCK
 ldir ;clear top row
 
 ld a,(linesClear)
 inc a
 ld (linesClear),a
 pop hl
 ret
 
newBlock:
 ld a,5
 ld (curX),a
 ld a,0
 ld (curY),a
 ld (curR),a
 ld (curStatus),a
 ld (timerT),a

 ld a,LOCK_DISABLE
 ld (lockTimer),a
 
 call getNextBagItem
 ld (curT),a
 ld ix,blockData
 ld d,a
 add a,a
 add a,a
 add a,a
 ld de,0
 ld e,a
 add ix, de
 ld a,(ix+spritePAL)
 ld a,(curT) ;ensure copied blockdata is CORRECT?
 call copyBlockData
 
 call checkBlockDownOK
 cp 0
 call z,gameEnd
 ;if block check fails immediately, it's a top out
 ret
 
;a=block type
determinedBlock:
 push af
 ld a,5
 ld (curX),a
 ld a,0
 ld (curY),a
 ld (curR),a
 ld (curStatus),a
 ld (lockTimer),a
 pop af
 ld (curT),a
 ld ix,blockData
 ld d,a
 add a,a
 add a,a
 add a,a
 ld de,0
 ld e,a
 add ix, de
 ld a,(ix+spritePAL)
 ld a,(curT) ;ensure copied blockdata is CORRECT?
 call copyBlockData
 ret
 
checkMoveLeft:
 ld a,(curX)
 ld hl, curBlock
 ld b,4
checkBlocksInLeft:
 ld c, (hl) ; c=xOfs
 ld a,(curX)
 add a,c ;a=x+xOfs
 cp 0
 jr z,blockTooFarLeft
 push bc
;check if block will collide
 push hl ;save the block data ptr
 ld c,(hl)
 inc hl
 ld b,(hl) ;(c,b) = (ofsx, ofsy)
 ld a,(curX)
 add a,c
 ld h,a
 ld a,(curY)
 add a,b
 ld l,a
 dec h
 call checkBlock
 cp NULL_BLOCK
 pop hl ;restore and inc 2 block data ptr
 pop bc
 jp nz, blockTooFarRight ;collision with block
 inc hl
 inc hl ;increment to next X
 djnz checkBlocksinLeft
 
 ;here: no blocks too far left...
 ld a,(curX)
 dec a
 ld (curX),a
 
 ;check if block needs lock
blockTooFarLeft:
 ld a, LOCK_DISABLE
 ld (lockTimer),a
 call checkBlockDownOK
 ret

lineKickLeft:
 ld hl, kicksI
 jr sharedKickLeft
 
checkRotationLeft:
 ld a,(curT)
 cp 3
 ret z
 cp 0
 jr z, lineKickLeft
 
 ld hl, kicksNormal
sharedKickLeft:
 push hl ;save table address
 
 ld a,(curR)
 cp 0
 jr nz,skipAdjust4
 ld a,4
skipAdjust4:
 ld (curR),a

 call getTableOffset ;de=offset
 pop hl ;restore table address
 add hl,de ;offset + table = starting rotation kick
 
 ld de,-10
 call calculateRXY ;hl is up two now, rxy stored
 push hl ;at next offset already
 
 ld a,0
 ld (rAttempt),a ;there are currently 0 rotation attempts.
 
 ld hl, curBlock
 ld b,4
 
checkBlocksRotateLeft:
 ld (rotationTempBC),bc
 ;get y
 ld a,(hl)
 neg
 ld e,a
 ld a,(rY)
 add a,e
 cp 20
 jr nc, noRotationLeft
 ld e,a
 
 ;get x
 inc hl
 ld a,(hl)
 ld d,a
 ld a,(rX)
 add a,d
 cp 10
 jr nc, noRotationLeft
 ld d,a
 
 ld (rotationTempHL),hl
 push de
 pop hl
 call checkBlock
 cp NULL_BLOCK
 jp nz,noRotationLeft
 ld hl,(rotationTempHL)
 inc hl
 ld bc,(rotationTempBC)
 djnz checkBlocksRotateLeft
;rotation is a go
 ld b,4
 ld hl, curBlock
rotateBlocksLeft:
 push bc
 ld a,(hl)
 neg
 ld e,a
 inc hl
 ld a,(hl)
 ld d,a
 dec hl ;back to X location
 ld (hl),d
 inc hl
 ld (hl),e ;blocks have rotated: now do the next one
 inc hl
 pop bc
 djnz rotateBlocksLeft
 pop hl
 
 ld a,(curR)
 dec a
 and $03
 ld (curR),a
 
 ld a,(rX)
 ld (curX),a
 ld a,(rY) ;save the location of the successful turn.
 ld (curY),a
 
 ld a, LOCK_DISABLE
 ld (lockTimer),a
 call checkBlockDownOK
 ret
noRotationLeft:
 pop hl ;get offset
 
 ld de,-10
 call calculateRXY ;rxy stored - hl is already at next ofs

 ld ix,rules
 bit rbitSRSEnabled,(ix+0)
 ret z ;SRS is disabled - do not try again

 push hl ;at next offset already 
 ld hl, curBlock
 ld b,4
 
 ld a,(rAttempt)
 inc a
 ld (rAttempt),a
 cp 5
 jp nz, checkBlocksRotateLeft

 pop hl ;unneeded.
 ld a, LOCK_DISABLE
 ld (lockTimer),a
 call checkBlockDownOK
 ret
 
rotationTempHL:
 .db 0,0,0 ;3 bytes for HL
rotationTempBC:
 .db 0,0,0 ;3 bytes for BC
rX:
 .db 0
rY:
 .db 0
rAttempt:
 .db 0

;multiplys curR by 10 and stores to de
getTableOffset:
 ld a,(curR)
 ld de,0
 ld hl,0
 ld e,a
 ld l,a
 add hl,hl
 add hl,hl
 add hl,de
 add hl,hl ;10*rotation -> hl is offset in table at saved (hl)
 push hl
 pop de
 ret 
 
;requires HL to be table ptr
;updates HL by two
calculateRXY:
 ld a, (curX)
 add a,(hl) ;add x offset
 add hl,de
 sub (hl) ;subtract next offset
 or a
 sbc hl,de ;undo the addition to next offset (back to start)
 ld (rX), a ;save to special location
 inc hl ;now on Y value
 ld a, (curY)
 add a,(hl) ;add y offset
 add hl,de ;get next offset
 sub (hl)
 or a
 sbc hl,de ;undo again
 ld (rY), a
 inc hl ;hl is 2 past previous offset ->
 ret
 
lineKickRight:
 ld hl, kicksI ;testing
 jr sharedKickRight
 
checkRotationRight:
 ld a,(curT)
 cp 3
 ret z
 cp 0
 jr z, lineKickRight
 
 ld hl, kicksNormal
sharedKickRight:
 push hl ;save table address
 call getTableOffset ;de=offset
 pop hl ;restore table address
 add hl,de ;offset + table = starting rotation kick
 
 ld de,10
 call calculateRXY ;hl is up two now, rxy stored
 push hl ;at next offset already
 
 ld a,0
 ld (rAttempt),a ;there are currently 0 rotation attempts.
 
 ld hl, curBlock
 ld b,4
checkBlocksRotateRight:
 ld (rotationTempBC),bc
 ;<x,y> to <y,-x>
 ;get newy=-oldx
 ld a,(hl) ;oldX
 ;neg ;-oldX
 ld e,a ;e=-oldx
 ld a,(rY) 
 add a,e ;rY-oldX = new y location
 cp 20
 jr nc, noRotationRight ;out of bounds
 ld e,a ;e=new y
 ;get x
 inc hl ;now (hl)=oldY
 ld a,(hl)
 neg
 ld d,a ;d=oldY
 ld a,(rX) ;a=rX
 add a,d ;rX+oldY = new x location
 cp 10
 jr nc, noRotationRight ;also out of bounds
 ld d,a
 ld (rotationTempHL),hl ;save block pointer
 push de
 pop hl ;d,e -> h,l =new coordinates
 call checkBlock
 cp NULL_BLOCK ;check if no block
 jp nz,noRotationRight ;if block, skip rotation
 ld hl,(rotationTempHL) 
 inc hl ;hl is now 2 past offset, to check next block
 ld bc,(rotationTempBC)
 djnz checkBlocksRotateRight
;rotation is a go
 ld b,4
 ld hl, curBlock
rotateBlocksRight:
 push bc
 ld a,(hl) ;curBlock x
 ld e,a ;e=new y
 inc hl ;to curblocky ptr
 ld a,(hl)
 neg
 ld d,a
 dec hl ;back to curblockX location
 ld (hl),d ;save new x
 inc hl
 ld (hl),e ;blocks have rotated: now do the next one
 inc hl
 pop bc
 djnz rotateBlocksRight
 pop hl ;unneeded next kick ofs
 
 ld a,(curR) ;increment angle
 inc a
 and $03 ;keep in bounds
 ld (curR),a
 
 ld a,(rX)
 ld (curX),a
 ld a,(rY) ;save the location of the successful turn.
 ld (curY),a
 
 ld a, LOCK_DISABLE
 ld (lockTimer),a
 call checkBlockDownOK
 ret
noRotationRight:
 pop hl ;get offset

 ld de,10
 call calculateRXY ;new rxy stored - hl is already at next ofs
 
 ld ix,rules
 bit rbitSRSEnabled,(ix+0)
 ret z ;SRS is disabled - do not try again

 push hl ;at next offset already 
 ld hl, curBlock
 ld b,4
 
 ld a,(rAttempt)
 inc a
 ld (rAttempt),a
 cp 5 ;compare rAttempt: if 5 attempts have hit do not jump back
 jp nz, checkBlocksRotateRight
 
 pop hl ;unneeded.
 ld a, LOCK_DISABLE
 ld (lockTimer),a
 call checkBlockDownOK
 ret
 
checkMoveRight:
 ld a,(curX)
 ld hl, curBlock
 ld b,4
checkBlocksInRight:
 ld c, (hl) ; c=xOfs
 ld a,(curX)
 add a,c ;a=x+xOfs
 cp 9
 jr z,blockTooFarRight
 push bc
 ;check if block will collide
 push hl ;save the block data ptr
 ld c,(hl)
 inc hl
 ld b,(hl) ;(c,b) = (ofsx, ofsy)
 ld a,(curX)
 add a,c
 ld h,a
 ld a,(curY)
 add a,b
 ld l,a
 inc h
 call checkBlock
 cp NULL_BLOCK
 pop hl ;restore and inc 2 block data ptr
 pop bc
 jr nz, blockTooFarRight ;collision with block
 inc hl
 inc hl ;increment to next X
 djnz checkBlocksinRight
 ;here: no blocks too far right...
 ld a,(curX)
 inc a
 ld (curX),a
 
 ;check if still needs lock timer
blockTooFarRight:
 ld a, LOCK_DISABLE
 ld (lockTimer),a
 call checkBlockDownOK
 ret

;checks if the block CAN move down AND moves the block if it can.
;note: affects lock timer
checkBlockDown:
 call checkBlockDownOK
 cp 0
 ret z ;check block down failed: no decrement
 call lowerBlock
 call checkBlockDownOK ;check if it can lower again for the purpose of lock timing.
 ret

;checks if the block CAN move down without actually moving it
;note: affects lock timer
checkBlockDownOK:
 ld hl, curBlock + 1
 ld b,4
checkBlocksInDown:
 ld c, (hl)
 ld a, (curY)
 add a,c
 cp 19
 jr z, blockTooFarDown
 push bc
 push hl
 ld c,(hl)
 dec hl
 ld b,(hl) ;(b,c) = (ofsx, ofsy)
 ld a,(curX)
 add a,b
 ld h,a
 ld a,(curY)
 add a,c
 ld l,a
 inc l
 call checkBlock
 cp NULL_BLOCK
 pop hl
 pop bc
 jr nz,blockTooFarDown ;block is hit
 inc hl
 inc hl
 djnz checkBlocksInDown
 ld a,1 ;success
 ret

blockTooFarDown:
 ld a,(lockTimer)
 cp LOCK_DISABLE
 jr nz,noLockRefresh
 ld a,(lockDelay)
 ld (lockTimer),a
noLockRefresh:
 ld a,0
 ret
 
lowerBlock:
 ld a,(curY)
 inc a
 ld (curY),a
 ret 

;inputs: 
;h = x
;l = y
;returns: a=block  hl=location
checkBlock:
 ld a,l ;y
 ld d,a ;y
 add a,a ;x2
 add a,a ;x4
 add a,d ;4y + 1y
 add a,a ;10y
 ld de,0 ;aaa
 ld e,h
 ld hl,0 ;;;aaaa
 ld l,a 
 add hl,de ;10Y +x
 ld de,field
 add hl,de
 ld a,(hl) ;a = block at (10Y+X)
 ret
 
;inputs:
;a = block type
copyBlockData:
 push af ;ld d,a ;save d for later
 ld hl, blockData
 add a,a ;x2
 add a,a ;x4
 add a,a ;x8
 ld de,0
 ld e,a ;de = offset from blockData
 add hl,de
 ;hl = pointer to blockdata
 ld de, curBlock
 ld bc, blockDataSize
 ldir ;copied to current block mem
 
 pop af ;get a again
 ld hl, blockGraphicData
 add a,a
 ld de,0
 ld e,a
 add hl,de ;blockGraphic + ofs
 ld de, curBlock + blockDataSize
 ld bc, blockGraphicSize
 ldir
 ret
 
;initialize 8bpp mode and palette
initLCD:
 call _RunIndicOff

 ld hl, PSS
 ld de, PSS + 1
 ld bc, 512
 ld (hl),0
 ldir
 
;intiialize palette from data
 ld hl, paletteData
 ld de, $e30200
 ld bc, paletteDataEnd - paletteData
 ldir  
 
 ;ld hl, lcdIntVSync 
 ;ld (mpLcdCtrl),hl
  
 ld hl, lcdBpp8 | lcdPwr | lcdBgr | lcdIntFront
 ld (mpLcdCtrl),hl
 ;following change based on ;https://www.cemetech.net/forum/viewtopic.php?t=13695&start=0
 ld a,4
 ld (mpLcdIcr),a
 
clearLCD:
 ld hl, vRam
 ld de, vRam+1
 ld bc, vRamEnd - vRam
 ld (hl),0
 ldir
 ret

;return lcd to normal state (bpp16)
resetLCD:
;set default vram ptr
 ld hl,vRam
 ld (mpLcdUpbase),hl
 
 ld hl, lcdNormalMode
 ld (mpLcdCtrl), hl
 call _ClrLCDFull
 call _DrawStatusBar
 ret

;swaps ptrs between two vram buffers.
;this is for double-buffering and preventing
;flicker/tears/etc.
vramOnPtr:
 .dl vRam
vramOffPtr:
 .dl vRamSplit
 
swapVRamPTR:
 ld de,(vramOffPtr)
 ld (mpLcdUpbase),de
 
swapVRamPTRNoDisp:
 ld hl,(vramOnPtr)
 ld de,(vramOffPtr)
 ld (vramOnPtr),de
 ld (vramOffPtr),hl
 
;also modified based on
;https://www.cemetech.net/forum/viewtopic.php?t=13695&start=0
waitLCDRIS:
 ld hl, mpLcdIcr
 set bLcdIntLNBU, (hl)
 ld l, mpLcdRis & $FF
wr:
 bit bLcdIntLNBU, (hl)
 jr z, wr
 ret

;input:
;a = color
;de = x
;l = y
;b = sizey
;c = sizex
;clears a square area with the color in a 
;note: preserves coords, sizexy
;this is so it can be used with sprites
clearSprite:
 push hl
 push de
 push bc
 push de
 ld h,0
 ld d,0 ;save HL = Y
 ld e,l
 add hl,hl ;2y
 add hl,hl ;4y
 add hl,de ;5y
 add hl,hl ;10y
 add hl,hl ;20y
 add hl,hl ;40y
 add hl,hl ;80y
 add hl,hl ;160y
 add hl,hl ;320y
 pop de
 add hl,de ;320Y+X
 ld de, (vramOffPTR) ;to second half of vRam, for double-buffering
 add hl,de ;320Y+x+vRam
 
 ld de,320
putClearYLoop:
 push bc
 ld b,c
 push hl ;save ptr to graph
putClearXLoop:
 ld (hl),a
 inc hl
 djnz putClearXLoop
 pop hl
 add hl,de ;move down 1 y unit
 pop bc
 djnz putClearYLoop
 pop bc
 pop de
 pop hl
 ret
 
drawSpriteCustomBPP:
 push hl
 push de
 ld de,0
 ld e,h
 ld hl,ppbpp+1 ;past jp
 add hl,de
 add hl,de
 add hl,de
 add hl,de ;hl = ppbpp+1 + 4*bppid
 ld de,(hl) ;get ppbpp data
 ld hl,smcPutPixel+1
 ld (hl),de ;put ppbpp data to call
 pop de
 pop hl
 jr drawSpriteSetBPP
;inputs: 
;ix = spritedataptr
;de = x
;l = y
;b = sizey
;c = sizex (note: BYTES not PIXELS)
;a = spritePalette
drawSprite:
 push de
 ld de, putPixel8bpp
 ld (smcPutPixel+1),de
 pop de
drawSpriteSetBPP:
 push de
 ld h,0
 ld d,0 ;save L = Y
 ld e,l
 add hl,hl ;2y
 add hl,hl ;4y
 add hl,de ;5y
 add hl,hl ;10y
 add hl,hl ;20y
 add hl,hl ;40y
 add hl,hl ;80y
 add hl,hl ;160y
 add hl,hl ;320y
 pop de
 add hl,de ;320Y+X
 ld de, (vramOffPtr) ;to second half of vRam, for double-buffering
 add hl,de ;320Y+x+vRam
;hl=vramptr ix=data b=sizeY c=sizeX a=pal 
putSpriteYLoop:
 push bc
 ld b,c
 push hl ;save ptr to graph
putSpriteXLoop:
 push bc
smcPutPixel: ;add 1 for CALL address
 call 0 ;ppbpp + something
 pop bc
 djnz putSpriteXLoop
 pop hl
 ld de,320
 add hl,de ;move down 1 y unit
 pop bc
 djnz putSpriteYLoop
 ret

;jp really only for padding right now
;so that indexes are *4 instead of *3
ppbpp:
 jp putPixel1bpp
 jp putPixel2bpp
 jp putPixel4bpp
 jp putPixel8bpp
 
;a=pal
;ix=data ptr
;hl=vram ptr
putPixel8bpp:
 push af
 ld a,(ix+0)
 cp 0 ;check if no color
 jp z,transPixel8
 pop af
 push af
 add a,(ix+0) ;add color to palette ofs
 ld (hl),a
transPixel8:
 pop af
 inc ix ;sprite data index
 inc hl ;coords
 ret
 
;a=pal
;ix=data ptr
;hl=vram ptr
putPixel4bpp:
 ld c,a ;save palette ofs
 ld b,(ix+0) ;get data
 ld a,b
 and $F0 
 rrca
 rrca
 rrca
 rrca ;shift to lower bits
 cp 0
 jr z, noPut1stPixel
 add a,c ;add palette ofs to color
 ld (hl),a ;load first nibble + palette ofs
noPut1stPixel:
 inc hl ;next coord
 
 ld a,b ;data again
 and $0F 
 cp 0
 jr z,noPut2ndPixel
 add a,c ;add palette and color
 ld (hl),a
noPut2ndPixel:
 inc hl ;next coord
 inc ix ;next data chunk
 ld a,c
 ret

;a=palette ofs
;hl=vram ptr
;ix=data ptr
putPixel2bpp:
 ld d,a ;save palette ofs
 ld c,(ix+0)
 ld b,4
putPixel2bit:
 xor a ;a=0
 rlc c
 rla
 rlc c
 rla ;get 2 bits from c into a
 cp 0
 jr z, trans2bit
 add a,d ;add pal and ofs
 ld (hl),a
trans2bit:
 inc hl
 djnz putPixel2bit
 inc ix
 ld a,d
 ret
 
;a=palette ofs
;ix=data ptr
;hl=vram ptr
putPixel1bpp:
 ld c,(ix+0)
 ld b,8
 ;new:
 ;c=bit data
 ;b=pixel count
putPixelBit:
 rlc c ;get bit from c
 ;if bit is zero, pixel is transparent.
 jr nc, noPutPixelBit ;skip to inc location
 ld (hl),a ;save to location
noPutPixelBit:
 inc hl ;next
 djnz putPixelBit
 inc ix ;done with this byte of data
 ret
 
;input: h
;destroys: b
;output: c = 1 << h
getBPPFromID:
 ld b,h
 inc b
 ld c,$01
shiftLoop:
 rlc c
 djnz shiftLoop
 rrc c
 ret
 
 ;draws a block from coords + set tSpriteId, tSpritePAL
 ;no bounds check 
 ;accepts (DE, L) as coordinates
 ;still needs tSpriteID and tSpritePAL
drawOneBlockNoGrid:
 ld ix, (drefSprite)
 ld h, (ix) ;get bpp
 inc ix
 ld bc,0
 call getBPPFromID
 ;h = bpp id
 ;c = bpp #
 
 ld a, (tSpriteID)
 ld b,144
 mlt bc ;12 * 12 * c
 srl b
 rr c
 srl b
 rr c 
 srl b
 rr c ;144 * c / 8 where c is in [1,2,4,8]
 ;therefore, c<256 and b can be mlt'd again
 ld b, a
 mlt bc ;spritedatasize*spriteID
 add ix,bc
 
 ld c,12 ;width in BYTES
 ld b,h
 inc b
divideByBPP:
 rrc c
 djnz divideByBPP
 ld b,12
 ld a, (tSpritePAL)
 call drawSpriteCustomBPP
 ;pop hl ;restore old coordinates
 ret

drawGame:
 ld ix,(drefIInfo)
 call drawObjectsNoReset
 
 ld hl, curStatus
 bit csClearLineBit, (hl)
 jr nz, skipDraw1
 ld hl, curStatus+midOfs
 bit csClearLineBit, (hl)
 jr nz, skipDraw1
 
 call eraseOldMino
 call drawNewMino

skipDraw1:
 call swapVRamPTR
 
 ld ix,(drefIInfo)
 call drawObjects
 
 ld hl, curStatus
 bit csClearLineBit, (hl)
 jr nz, skipDraw2
 ld hl, curStatus + midOfs
 bit csClearLineBit, (hl)
 jr nz, skipDraw2

 call eraseOldMino
 call drawNewMino

skipDraw2:
 ret
 
gameEndInit:
 call checkBest

 ld ix,(drefIInfo)
 call resetAllDrawCheck
 ld ix,(drefIInfo)
 call drawObjectsNoReset
 ld ix,(drefGameOver)
 call drawObjectsNoReset
 call swapVRamPTR
 ;draw to 2nd buffer
 ld ix,(drefIInfo)
 call drawObjectsNoReset
 ld ix,(drefGameOver)
 call drawObjectsNoReset
 call swapVRamPTR
 
 
gameEndWait:
 call scanKeys

 ld ix,mbuttonConfirm
 call checkKeyDown
 jp c,mainmenu ;go back to main menu
 
 ld ix,mbuttonQuit
 call checkKeyDown
 jp c,exit ;end game
 jr gameEndWait
 
checkBest:
 ld ix,rules
 bit rbitLowTime,(ix+rfScore)
 jr nz, checkLowTime
 ;unneeded due to order:
 ;bit rScore,(ix+rfScore)
 ;jr checkHighScore
 
checkHighScore:
 ld hl,(highscore)
 ld de,(score)
 or a,a
 sbc hl,de
 ;carry implies highscore < score 
 ret nc ;return if no high score
 ;set high score
 ld hl,(score)
 jr setBestScore
 
checkLowTime:
 ld hl,(highscore)
 ld de,0
 scf
 sbc hl,de
 jr c, setFirstTime;if subtracting 1 results in carry, highscore must be 0 and therefore invalid, so just set the current score.
 
 ld hl,(highscore)
 ld de,(globalTimer)
 or a,a
 sbc hl,de
 ret c ;c implies best time < current time: no record
 ;set low time
setFirstTime:
 ld hl,(globalTimer)
 jr setBestScore
 
setBestScore:
 ld a,(ix+rMode)
 ld ix,(saveDataPTR)
 ld de,0
 add a,a
 add a,a
 ld e,a
 add ix,de ;add 4
 add ix,de ;add 8
 ld de,savHighscore
 add ix,de ;ix = savedata + hscoreofs + modeofs
 ld (highscore),hl ;store score
 ld (ix),hl
 ret
 
eraseOldMino:
 ld hl, curStatus + midOfs
 bit csLockedBit, (hl)
 ret nz ;don't clear a locked mino
 
 ld ix,(refField)
 ld hl, midData
 call NullPTRMino
 ret
 
drawNewMino:
 ld ix,(refField)
 ld hl, curData
 call drawMinoFromPTR
 ret
 
;input: ix = info ptr, 1st elem. is # struct elems
drawObjects:
 ld a,$00 ;nop
 jr drawObjs
drawObjectsNoReset:
 ld a,$C9 ;ret
 ;jr drawObjs

drawObjs:
 ld (smcRedrawReset),a
 ld b,(ix)
 inc ix 
drawAllObjects:
 push bc
 ;ix = info ptr
 call drawObject
 
 ld de,iDataSize
 add ix,de
 pop bc
 djnz drawAllObjects
 ret

;ix = info ptr, 
resetAllDrawCheck:
 ld b,(ix)
 inc ix
 ld de,iDataSize
resAllObjects:
 res redrawObjBit,(ix+iDataType)
 add ix,de
 djnz resAllObjects
 ret
 
drawObjectNoReset:
 ld a,$c9 ;ret = $c9
 ld (smcRedrawReset),a
;input: ix = data ptr to object
;note: no guarantees on data ptr validity on return
;note: only sets 1st bit
drawObject:
 push ix
 call drawObjectJump
 pop ix
 ;cleanup from drawing object
smcRedrawReset:
 ret ;either ret (no set) or nop (set)
 
 set redrawObjBit,(ix+iDataType)
 ret
 
drawObjectJump:
 ld de,0
 ld e, (ix+iDataType) ;e is data type
 sla e
 ret c ;return if already drawn
 sla e
 
 ld hl,drawJumps
 add hl,de ;jumps + 4e (since e is already *4 from 2x sla e)
 
 jp (hl)
 ret ;implied return from any of the jump methods
 
;jump table for various object types
;all should accept ix as a pointer to data
drawJumps:
 jp drawField ;includes current mino
 jp drawText
 jp draw24BitNumber
 jp drawSpriteObject
 jp drawCurrentHold
 jp drawPreview
 jp drawBox
 jp drawMenu
 ld h,sp8bpp
 jr sharedDSO
 ld h,sp4bpp
 jr sharedDSO
 ld h,sp2bpp
 jr sharedDSO
 ld h,sp1bpp
 jr sharedDSO
 jp draw8BitNumber
 ret ;this one is just aesthetic
 
sharedDSO:
 jp drawSpriteObj

drawNullBlock:
 ld a,0
 ld (tSpriteID),a
 ld (tSpritePAL),a
 jr drawNullBlockReturn
 
drawField:
 ld b,(ix+iDataH)
drawFieldY:
 push bc
 ld c,b
 ld b,(ix+iDataW) ;(b,c) = (w,h)
drawFieldX:
 push bc
 ld h,b
 dec h
 ld l,c
 dec l
 push hl
 call checkBlock
 cp NULL_BLOCK
 jr z,drawNullBlock
 ld a,(hl) ;a is now curT
 ld d,a
 add a,a ;2
 ld de,0
 ld e,a
 ld hl,blockGraphicData
 add hl,de ;find block sprite ID and pal from here
 ld a,(hl)
 ld (tSpriteID),a
 inc hl ;spritePAL
 ld a,(hl)
 ld (tSpritePAL),a
drawNullBlockReturn:
 pop hl ;use same grid coords, but operate differently
 ld e,h ;save e=x
 ld h,12
 mlt hl ;hl = 12*y implies l=12*y since y<20
 ld d,12
 mlt de ;de = 12*(x + xOfs)
 ;(de, l) are the coordinates
 ;now, layout offset must be added.
 ld a,(ix + iDataXL)
 add a,e
 ld e,a  ;12x+ layoutOfsX
 ld a,(ix + iDataXH)
 adc a,d ;add high byte x ofs - include carry for completeness
 ld d,a
 ld a,(ix + iDataY)
 add a,l ;no bounds check! careful!
 ld l,a
 ;de = 12xgrid+lofsx
 ;l  = 12ygrid+lofsy
 ;tSprite stuff is OK
 push ix ;preserve info ptr
 ;following code is transparency fix
 ;OLD---not in use because it lags pretty badly
 ;NEW---due to revised draw code, transparency fix
 ;is now active. remove if you have lag
 ;and have no transparent blocks.
 ld a,(tSpriteID)
 ld b,a
 ld a,(tSpritePAL)
 ld c,a
 ld a,0
 ld (tSpriteID),a
 ld (tSpritePAL),a
 push bc ;tSprite info
 push hl ;y location
 push de ;x location
 call drawOneBlockNoGrid
 pop de
 pop hl
 pop bc
 ld a,b
 ld (tSpriteID),a
 ld a,c
 ld (tSpritePAL),a
 call drawOneBlockNoGrid
 pop ix ;restore info ptr
skipBlockDraw:
 pop bc
 djnz drawFieldX
 pop bc
 djnz drawFieldY
 
 ;push ix
 ;call drawCurrentMino
 ;pop ix
 ret
 
drawMinoTempDE:
 .db 0,0,0
dmX:
 .db 0
dmY:
 .db 0
dmOX:
 .db 0,0
dmOY:
 .db 0

drawCurrentHold:
 push ix ;save data ptr
 ld ix,rules
 bit rbitHoldEnabled,(ix+0)
 pop ix ;restore data ptr/stack
 ret z ;hold is disabled
 
 ld a,0
 ld (tSpriteID),a
 ld (tSpritePAL),a
 
 ld b,(ix+iDataH)
drawHoldY:
 push bc
 ld c,b
 ld b,(ix+iDataW) ;(b,c) = (w,h)
drawHoldX:
 push bc
 ld h,b
 dec h
 ld l,c
 dec l
 ld e,h ;save e=x
 ld h,12
 mlt hl ;hl = 12*y implies l=12*y since y<20
 ld d,12
 mlt de ;de = 12*(x + xOfs)
 ;(de, l) are the coordinates
 ;now, layout offset must be added.
 ld a,(ix + iDataXL)
 add a,e
 ld e,a  ;12x+ layoutOfsX
 ld a,(ix + iDataXH)
 adc a,d ;add high byte x ofs - include carry for completeness
 ld d,a
 ld a,(ix + iDataY)
 add a,l ;no bounds check! careful!
 ld l,a

 push ix ;preserve data ptr
 call drawOneBlockNoGrid
 pop ix ;restore data ptr
 pop bc
 djnz drawHoldX
 pop bc
 djnz drawHoldY

 ld a,(holdT)
 cp NULL_BLOCK
 ret z ;don't draw if it's empty!
 
 ld a,(ix+iDataXL)
 ld (dmOX),a
 ld a,(ix+iDataXH)
 ld (dmOX+1),a
 ld a,(ix+iDataY)
 ld (dmOX+2),a
 
 ld a,1
 ld (dmX),a
 ld a,2
 ld (dmY),a
 ;(1,2) optimally centers piece in hold box
 
 ld a,(holdT)
 add a,a
 ld hl, blockGraphicData
 ld de,0
 ld e,a
 
 add hl, de ;block graphic data ptr
 ld a, (hl)
 ld (tSpriteID),a
 inc hl ;block data + id + pal
 ld a, (hl)
 ld (tSpritePAL),a

 ld hl, blockData
 sla e
 sla e
 add hl,de ;points to block data
 ex de,hl
 
 jr drawAnyMino

;inputs:
;ix = obj data ptr
;hl = ptr to curxy etc.

drawMinoFromPTR:
 call setDataFromPTR
 jr drawAnyMino
 
;inputs:
;ix = obj data ptr
;hl = ptr to curxy etc.
setDataFromPTR:
 ld a,(hl)
 ld (dmX),a
 inc hl
 ld a,(hl)
 ld (dmY),a
 inc hl ;past XY
 
 ld a,(ix+iDataXL)
 ld (dmOX),a
 ld a,(ix+iDataXH)
 ld (dmOX+1),a
 ld a,(ix+iDataY)
 ld (dmOY),a 
 
 inc hl ;past rotation
 ld a,(hl) ;save type
 inc hl ;past type
 inc hl ;past status
 push hl ;now at block data struct
 ld de,spriteID
 add hl,de ;hl points to spriteID in block data struct
 ld a,(hl)
 ld (tSpriteID),a
 inc hl
 ld a,(hl)
 ld (tSpritePal),a
 
 pop de
 ret
 
;requires ix as obj data ptr
;hl as setDataFromPTR ptr to curdata etc
;also, nice
nullPTRMino:
 call setDataFromPTR
 ld a,0
 ld (tSpriteID),a
 ld (tSpritePAL),a
 jr drawAnyMino
 
drawCurrentMino:
 ld hl, curData
 call setDataFromPTR
 ;jr drawAnyMino

;inputs: de=block data struct
;ix = obj data ptr
;dmx,dmy,dmox,dmoy
drawAnyMino: 
 ld b,4
 ld (drawMinoTempDE),de
drawMinoBlocks:
 push bc
 
 ld de,(drawMinoTempDE)
 ld hl,0
 ld a,(dmX)
 ld l,a
 ld a,(de) ;x+ofsx
 add a,l
 cp (ix+iDataW)
 jr nc,skipDrawBlock
 ld l,a
 ld h,12
 mlt hl ;12x
 ld a,(dmOX+0)
 add a,l ;
 ld l,a
 ld a,(dmOX+1)
 adc a,h
 ld h,a ;hl=12x+12o+layoutofsx
 push hl ;save x
 inc de
 
 ld hl,0
 ld a,(dmY)
 ld l,a
 ld a,(de)
 inc de
 ld (drawMinoTempDE),de
 add a,l
 pop bc ;stack dancing
 cp (ix+iDataH)
 jr nc,skipDrawBlock
 push bc ;around a skip
 ld l,a
 ld h,12
 mlt hl ;12x
 ld a,(dmOY) ;hmm
 add a,l
 ld l,a ;hl=y location
 pop de ;de=x location
 
 push ix
 call drawOneBlockNoGrid
 pop ix
skipDrawBlock:
 pop bc
 djnz drawMinoBlocks
 ret

drawTextColor:
 .db 0
drawTextBG:
 .db 0

;inputs: ix = data ptr
drawText:
 ld bc,SSS
 ld hl,0
 ld h,(ix+iDataPTRH)
 ld l,(ix+iDataPTRL)
 add hl,bc
 push hl
 
 ld de,0
 ld e,(ix+iDataXL)
 ld d,(ix+iDataXH)
 ld hl,0
 ld l,(ix+iDataY) ;xy set
 ld a,(ix+iDataA) ;color
 ld c,(ix+iDataH) ;bg color 0=transparent
 pop ix ;this is now the string data ptr

 ;must be provided:
 ;ix - string data ptr
 ;hl - y
 ;de - x
 ;a - color
 ;c - bg color
drawTextManual:
 ld (drawTextColor),a
 ld a,c
 ld (drawTextBG),a
drawTextLoop:
 ld a,(ix)
 cp 0 ;check null-terminated string
 jr z,textLoopEnd ;end string before stuff on stack
 push ix ;string data ptr
 push hl
 push de

 ;a = ofs
 sub 32 ;ascii adjust
 ld ix,(drefFont)
 ld h,(ix) ;bpp
 inc ix ;ix points to fontdata
 ld bc,0
 ld b,h
 inc b
 ld c,$08
shiftCTXT:
 rlc c
 djnz shiftCTXT
 rrc c
 ;c=2^bpp*8
 ld b,a
 mlt bc ;bc = spdatasize * ofs
 
 add ix,bc ;fontData + 64a (character)

 ld b,8
 ld c,8 ;size in PIXELS of char BACKGROUND width
 
 ld a,(drawTextBG)
 cp 0
 jr z,textTransBG
 push hl
 call clearSprite
 pop hl
textTransBG:
 ld a,(drawTextColor)
 ld b,h
 inc b
 ld c,$80
shiftLoopC:
 rlc c
 djnz shiftLoopC ;c is sprite width in BYTES
 ld b,8 ;sprite height
 ;h is bpp, l=y, de=x, ix=spdata
 call drawSpriteCustomBPP
 pop hl ;de, x coords
 ld de,8
 add hl,de ;add 8
 push hl
 pop de ;de = x+8
 pop hl ;y value (shouldn't change)
 pop ix ;string data ptr
 inc ix
 jr drawTextLoop
textLoopEnd:
 ;de is the ending X location
 ;hl is unchanged
 ;ix points to 0 at end of string
 ;a is 0
 ret
 
;inputs: ix = data struct
drawNumString:
.db 0,0,0,0,0,0
.db 0,0,0,0,0,0 ;12 bytes max
.db 0
draw8BitNumber:
 ld hl,0
 ld h,(ix+iDataPTRH)
 ld l,(ix+iDataPTRL)
 ld bc,PSS
 add hl,bc ;ptr to number
 ld a,(hl)
 or a,a
 sbc hl,hl
 ld l,a ;hl = number
 ;jump to shared code
 jr drawNumShared

draw24BitNumber:
 ld hl,0
 ld h,(ix+iDataPTRH)
 ld l,(ix+iDataPTRL)
 ld bc,PSS
 add hl,bc ;ptr to number
 
 ld hl,(hl) ;hl is number
 
drawNumShared:
 ld de,drawNumString+11
 ld b,12
convToString:
 push bc
 ld a,10
 call _DivHLByA
 add a,48
 ld (de),a
 dec de
 pop bc
 djnz convToString ;a=0 is end of loop
 
 ld bc,0
 ld c,(ix+iDataW)
 ld hl,drawNumString+12
 or a,a
 sbc hl,bc
 push hl
 
 ld de,0
 ld e,(ix+iDataXL)
 ld d,(ix+iDataXH)
 ld hl,0
 ld l,(ix+iDataY) ;xy set
 ld a,(ix+iDataA) ;color
 ld c,(ix+iDataH) ;bg color 0=transparent
 pop ix ;string data ptr
 
 call drawTextManual
 ret

;ix = input
drawSpriteObject:
 ld h,sp8bpp ;default 8bpp
drawSpriteObj:
 ld a,h ;save bpp
 push ix
 ld hl,0
 ld h,(ix+iDataPTRH)
 ld l,(ix+iDataPTRL)
 ld bc,SSS
 add hl,bc ;ptr to number
 push hl ;ptr to sprite data
 
 ld hl,0
 ld h,a ;get bpp
 ld l,(ix+iDataY)
 
 ld de,0
 ld d,(ix+iDataXH)
 ld e,(ix+iDataXL)
 
 ld bc,0
 ld b,(ix+iDataH)
 ld c,(ix+iDataW)
 ld a,(ix+iDataA) 

 pop ix
 call drawSprite
 pop ix
 ret
 
;ix is ptr to input data
drawBox:
 ld de,0
 ld d,(ix+iDataXH)
 ld e,(ix+iDataXL)
 ld hl,0
 ld l,(ix+iDataY)

 push de
 ld h,0
 ld d,h ;save HL = Y
 ld e,l
 add hl,hl ;2y
 add hl,hl ;4y
 add hl,de ;5y
 add hl,hl ;10y
 add hl,hl ;20y
 add hl,hl ;40y
 add hl,hl ;80y
 add hl,hl ;160y
 add hl,hl ;320y
 pop de
 add hl,de ;320Y+X
 ld de, (vramOffPtr)
 add hl,de ;320Y+x+vRam
 
 ld de,320
 ld b,(ix+iDataH)
drawBoxYLoop:
 push bc
 ;check if y is max or 0
 ld a,b
 cp (ix+iDataH)
 jr z,isBorder
 cp 1
 jr z,isBorder
 ld a,(ix+iDataA)
drawBorderReturn:
 ld bc,0
 ld b,(ix+iDataPTRH)
 ld c,(ix+iDataPTRL)
 push hl ;save ptr to graph
drawBoxX:
 push hl
 pop de ;de = ptr to graph
 inc de ;hl=gptr, de=gptr+1
 ;bc is width of box
 ld (hl),a
 ldir
 ;end of drawBoxX
 ld a,(ix+iDataW) ;border color
 ld (hl),a
 pop hl ;restore old gptr
 ld a,(ix+iDataW) ;border color
 ld (hl),a
 
 ld de,320
 add hl,de ;move down 1 y unit
 pop bc
 djnz drawBoxYLoop
 ret
isBorder:
 ld a,(ix+iDataW)
 jr drawBorderReturn
 
;ix = data ptr input
;intended to perform initial draw of menu data
;output: ix = beginning of jumps/end of menu text
drawMenu:
 ld b,(ix+iDataW)
 
 ld hl,0
 ld h,(ix+iDataPTRH)
 ld l,(ix+iDataPTRL)
 ld de,SSS
 add hl,de ;hl points to text data of menu
 push hl
 
 ld b,(ix+iDataW) ; # items
 ld hl,0
 ld l,(ix+iDataY)
 ld de,0
 ld d,(ix+iDataXH)
 ld e,(ix+iDataXL) ;de, hl is (X,Y) coords
 
 ld a,(ix+iDataA)
 pop ix ;points to text data
drawMenuItems:
 push bc
 push de ;x value
 push af ;color value
 ld c,0 ; no bg color
 call drawTextManual
 inc ix
 ld a,8
 add a,l
 ld l,a ;add 8 to y value
 pop af ;restore color value
 pop de ;restore old x value
 
 pop bc
 djnz drawMenuItems
 ;ix should point to just past end of text data
 ret

menuSelection:
.db 0
menuDataPTR:
.dl 0
menuPTR:
.dl 0
cursorPTR:
.dl 0
  
;input:
;ix is ptr to active display menudata
activeMenu:
 call clearLCD
 ld a,0
 ld (menuSelection),a
 
 ld (menuDataPTR),ix
 inc ix ;go past # elements
 
 ;scan for typeMenu and read for data
 ld de,iDataSize
menuScanMenu:
 ld a,(ix+iDataType)
 add ix,de
 cp typeMenu
 jr nz,menuScanMenu
 ld de,-iDataSize
 add ix,de ;ix - iDataSize
 ;ix points to a typeMenu object
 ld (menuPTR),ix
 
 ld b,(ix+iDataW) ;# text items
 ld hl,0
 ld h,(ix+iDataPTRH)
 ld l,(ix+iDataPTRL)
 ld de,SSS
 add hl,de ;hl=ptr to text
scanToMenuJumps:
 ld a,(hl)
 inc hl
 cp 0
 jr nz,scanToMenuJumps ;jump no dec if not end of string
 djnz scanToMenuJumps
 ;hl points to jump table
 ld (smcLoadJumpTable+1),hl
 
 ld b,(ix+iDataH)
 ld c,iDataSize
 mlt bc ;bc=size*cursorelemid
 ld hl,(menuDataPTR)
 inc hl
 add hl,bc ;hl=ix+size*cursorID or, offset to cursor data
 ld (cursorPTR),hl
 
 ;wait for no button forward or back through menu
 ld ix,mbuttonConfirm
 call waitNoButton
 ld ix,mbuttonBack
 call waitNoButton
 jr menuDraw
 
menuLoop:
 call scanKeys
 
 ld ix,mbuttonConfirm
 call checkKeyDown
 jp c,menuSelect
 
 ld ix,mbuttonBack
 call checkKeyDown
 jp c,amenuEnd
 
 ld ix,mbuttonUp
 call checkKeyDown
 jr c,menuUp
 
 ld ix,mbuttonDown
 call checkKeyDown
 jr c,menuDown
 
 ld ix,mbuttonQuit
 call checkKeyDown
 jp c,exit ;early exit option (will end program)
 jr menuLoop ;only redraw if something happens

menuDraw:
 ;draw active items
 ld a,(menuSelection)
 add a,a
 add a,a
 add a,a ;8*selection
 ld ix,(menuPTR) ;ix points to cursor struct
 add a,(ix+iDataY) ;menuY + selection ofs
 ld ix,(cursorPTR)
 ld (ix+iDataY),a ;save y value to cursor
 
 ld ix,(menuDataPTR)
 call drawObjectsNoReset ;never stop drawing menu objects. 
 call swapVRamPTR
 ld ix,(menuDataPTR)
 call drawObjectsNoReset
 
 jr menuLoop
 
menuUp:
 ld a,(menuSelection)
 cp 0
 jr z, menuDraw ;don't go past zero
 dec a
 ld (menuSelection),a
 jr menuDraw
 
menuDown:
 ld a,(menuSelection)
 inc a
 ld ix,(menuPTR)
 cp (ix+iDataW) ;# items
 jr nc, menuDraw ;don't go past end of list
 ld (menuSelection),a
 jr menuDraw

amenuEnd:
 ;uh
 ld a,$FF
 ld (menuSelection),a ;yeah
 
menuSelect: 
 ld de,0
 ld a,(menuSelection)
 inc a
 ld e,a
 
smcLoadJumpTable:
 ld hl,$000000 ;this will be replaced
 add hl,de
 add hl,de
 add hl,de
 add hl,de
 
 jp (hl)
 
;uses (menuSelection) and (menuPTR) to get string ptr
getStringPTRSelection:
 ld ix,(menuPTR)
 
 ld hl,0
 ld h,(ix+iDataPTRH)
 ld l,(ix+iDataPTRL)
 ld de,SSS
 add hl,de
 
 ld a,(menuSelection)
 or a,a
 jr z, ptrOK ;selection is found
 
 ld b,a
scanMenuText:
 ld a,(hl)
 inc hl
 or a,a ;cp 0
 jr nz, scanMenuText
 ;gets here after a 0 is found
 djnz scanMenuText
 
ptrOK:
 ;pointer is found to string
 ;hl = pointer
 or a,a
 sbc hl,de ;hl=ofs from SSS
 ret
 
numberSelection:
.db 0
numberMax:
.db 0
setNumberPTR:
.dl 0
setVarPTR:
.dl 0
;setVarPTR is the pointer to the variable being
;modified by setNumber.
;or, what is being "selected."

setNumber:
 ;ix points to data
 ld (numberMax),a
 ld (setNumberPTR),ix

 or a,a
 sbc hl,hl ;hl=0
 ld h,(ix+iDataPTRH)
 ld l,(ix+iDataPTRL)
 ld de,PSS
 add hl,de
 ld (setVarPTR),hl ;points to variable to set
 
 ld a,(hl) ;get default/previous from memory
 ld (numberSelection),a
 
 ld ix, mbuttonConfirm
 call waitNoButton ;wait for no confirmation press
 ld ix, mbuttonBack
 call waitNoButton ;wait for no back press
 
setNumberLoop:
 call scanKeys
 
 ld ix,mbuttonBack
 call checkKeyDown
 jr c, setNumberFinal ;return from setNumber

 ld ix,mbuttonConfirm
 call checkKeyDown
 jr c, setNumberFinal ;return from setNumber
 
 ld ix,mbuttonLeft
 call checkKeyDown
 jr c, setNumDown
 
 ld ix,mbuttonRight
 call checkKeyDown
 jr c, setNumUp

 ld ix,mbuttonQuit
 call checkKeyDown
 jp c,exit ;early exit option (will end program)
 jr setNumberLoop ;only redraw if something happens
 
setNumDown:
 ld a,(numberSelection)
 dec a
 cp 255
 jr z, setNumDraw ;don't dec past 0
 ld (numberSelection),a
 jr setNumDraw
 
setNumUp:
 ld a,(numberMax)
 ld b,a
 ld a,(numberSelection)
 inc a
 cp b
 jr nc, setNumDraw
 ld (numberSelection),a
 ;jr setNumDraw
 
setNumDraw:
 call updateSetNum
 
 ld ix,(setNumberPTR)
 call drawObjectNoReset
 call swapVRamPTR
 ld ix,(setNumberPTR)
 call drawObjectNoReset
 jr setNumberLoop
 
updateSetNum:
 ld hl,(setVarPTR)
 ld a,(numberSelection)
 ld (hl),a
 ret
 
setNumberFinal: ;just wait for confirm/back to be released, then end
 ld ix, mbuttonBack
 call waitNoButton
 ld ix, mbuttonConfirm
 call waitNoButton
 ret ;return from setNumber call
 
randsav:
 .db 0,0,0,0
randbag:
 .db 0,0,0,0,0,0,0 ;7 slots for the 7 pieces
randbag2:
 .db 0,0,0,0,0,0,0 ;backup bag for next 7
RANDOM_NULL = 7 ;empty slot

getNextBagItem:
 ld a,(randbag) ;get first item
 push af
 ld hl,randbag+1
 ld de,randbag
 ld bc,13
 ldir ;slide all previous items up in queue
 ld hl,randbag2 + 6
 ld (hl),RANDOM_NULL
 ld a,(randbag2)
 cp RANDOM_NULL ;check that randbag2 is not empty
 ld hl,randBag2
 call z,randFillBag ;if the bag is empty, fill it.
 pop af
 ret

initBag:
 call randInit
 
 ld hl, randbag
 call randFillBag
 ld hl, randbag2
 call randFillBag
 ret

;input:
;hl= ptr to bag to fill 
randBagPtr:
 .db 0,0,0 ;save ptr to bag here
randCountFail:
 .db 0
randFillBag: 
 ld b,7
 ld (randBagPtr),hl
randFillLoop:
 push bc
 push hl
randTryAgain:
 call rand
 ld a,e
 and $07
 cp RANDOM_NULL
 jr z, randTryAgain ; seven is not an OK random number
 ld b, 7
 ld hl, (randBagPtr)
randCheckLoop:
 cp (hl)
 jr z, randTryAgain
 inc hl
 djnz randCheckLoop
 pop hl
 ld (hl),a ;value is good; save
 inc hl ;next item
 pop bc
 djnz randFillLoop
 ret

randseed:
 .db 0,0,0
randinput:
 .db 0
 
;initialize random stuff
randInit:
 ;init randseed
 ld a,r
 ld (randseed),a
 ld a,r
 add a,42
 ld (randseed+1),a
 
 ;init bags with RANDOM_NULL
 ld hl, randbag
 ld de, randbag+1
 ld bc, 13
 ld (hl), RANDOM_NULL
 ldir
 ret
 

;generates a random 16-bit nubmer
;galosis LFSR, based on wikipedia page
;en.wikipedia.org/wiki/Linear-feedback_shift_register
;Just read the wikipedia page.
;This version is a slightly modified version of a bugged z80 version I created a few years ago.
rand:
 ld de,(randseed)
 ld a,(randinput)
 bit 0,a
 call nz,randXOR
 ld a,(randinput)
 ld l,a
 push hl
 pop af
 rr d
 rr e
 push af
 pop hl
 ld a,l
 ld (randinput),a
 ld (randseed),de
 ret
randXOR:
 ld a,%10110100 ;this is the ideal set of bits to invert for a maximal cycle, apparently. (for 16 bits anyway)
 xor d
 ld d,a
 ret
 
;key registers read to here
keys:
.db 0,0,0,0
.db 0,0,0,0

applyTheme:
 ld a,(theme) ;value just set here
 ld hl,blockGraphicData
 ld b,7
replaceBlockSpriteID:
 ld (hl),a
 inc hl ;palette
 inc hl ;next block spriteid
 djnz replaceBlockSpriteID
 ;all spriteid has been replaced, back to option
 ret

;will be used in button mapping
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
 .db "ON",0 ;unobtainable
 .db "SOT",0
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
 .db "",0
;g6
 .db "ENTER",0
 .db "+",0
 .db "-",0
 .db "*",0
 .db "/",0
 .db "^",0
 .db "CLEAR",0
 .db "",0
;g7
 .db "DOWN",0
 .db "LEFT",0
 .db "RIGHT",0
 .db "UP",0
;.db "",0 ;does not exist
;.db "",0
;.db "",0
;.db "",0

;key ids for important operations
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

defaultButtonData:
PSS1024CopiedData:
;buttonleft:
.db 49, 7, 2, 0
;buttonright:
.db 50, 7, 2, 0
;buttonsoft:
.db 48, 0, 3, 0
;buttonhard:
.db 51, noRepeat, noRepeat, 0
;buttonrotateleft:
.db 5, noRepeat, noRepeat, 0
;buttonrotateright:
.db 15, noRepeat, noRepeat, 0
;buttonhold:
.db 23, noRepeat, noRepeat, 0
;buttonpause:
.db 6, noRepeat, noRepeat, 0

;buttonup:
.db 51, 60, 15, 0
;buttondown:
.db 48, 60, 15, 0
;buttonleft:
.db 49, 60, 15, 0
;buttonright:
.db 50, 60, 15, 0

;buttonconfirm:
.db 5, noRepeat, noRepeat, 0
;buttonback:
.db 15, noRepeat, noRepeat, 0
;buttonquit:
.db 7, noRepeat, noRepeat, 0
defaultButtonDataEnd:
PSS1024CopiedDataEnd:
defaultButtonDataSize = defaultButtonDataEnd - defaultButtonData

;ix = ptr to key data to check
checkKeyDown:
 ld a,(ix+buttonID)
 ld de,0
 ld e,a
 and $07 ; a = bit
 srl e
 srl e
 srl e  ; e = byte
 ld hl, keys
 add hl,de ;add byte ofs 
 ld b,a
 ld a,(hl) 
 inc b 
 ;b is shift count
 ;a is data to shift
shiftKeyBit:
 rrca
 djnz shiftKeyBit
 rlca ;result: whatever stuff this does
 ;so carry is set or not
 push af ;save carry state
 ld a,(ix+buttonTimer)
 cp 0
 jr z, buttonOK
 cp (ix+buttonTimeStart)
 jr c, buttonNotOK ;less than start time
 sub (ix+buttonTimeStart) ;remove start time
 cp (ix+buttonTimeRepeat) ;compare to repeater time
 jr c, buttonNotOK ;less than repeat: don't repeat
 ;hit repeat timer: reset it, continue, key is good
 ld a,(ix+buttonTimeStart)
 ld (ix+buttonTimer),a
 
buttonOK:
 pop af ;use whatever state button had
 
 jr nc, resetTimer
 inc (ix+buttonTimer)
 ret
 
buttonNotOK:
 pop af
 jr nc, resetTimer ;button isn't pressed anyways
 inc (ix+buttonTimer) ;inc for eventual repeat
 
 or a,a ;clear carry
 ret

resetTimer:
 ld a,0
 ld (ix+buttonTimer),a
 ret

;ix points to button info
;wait for no press of selected button
;note: ix will still point to button data after
;being called
waitNoButton:
 call scanKeys

 call checkKeyDown
 jr c, waitNoButton ;wait for no selection
 ret
 
;waits for button press
;ix points to button info
;note: ix will still point to button data after
;function ends
waitButton:
 call scanKeys

 call checkKeyDown
 jr nc, waitButton ;wait for selection
 ret 

initKeyTimer:
 ld b,64
rkeys:
 push bc
 ld a,b
 dec a
 call checkKeyDown
 pop bc
 djnz rkeys
 ret
 
;source: http://wikiti.brandonw.net/index.php?title=84PCE:Ports:A000
;I get how this works in theory
;but it's still sketchy to me
;so I copied it directly
scanKeys:
 di             ; Disable OS interrupts
 ld hl,0F50000h
 ld (hl),2      ; Set Single Scan mode

 xor a,a
scan_wait:
 cp a,(hl)      ; Wait for Idle mode
 jr nz,scan_wait

 ;just take all of the keys
 ;probably can loop but eh
 ld a,(kbdG1)
 ld (keys),a
 ld a,(kbdG2)
 ld (keys+1),a
 ld a,(kbdG3)
 ld (keys+2),a
 ld a,(kbdG4)
 ld (keys+3),a
 ld a,(kbdG5)
 ld (keys+4),a
 ld a,(kbdG6)
 ld (keys+5),a
 ld a,(kbdG7)
 ld (keys+6),a
 ;ld a,(kbdG8) ;oh wait
 ;ld (keys+7),a

 ei             ; Enable OS interrupts
 ret
 
;source: http://wikiti.brandonw.net/index.php?title=84PCE:Ports:A000
;this is similarily copied from the same source as above
;but I really don't know what this does internally,
;so I'm copying and not changing anything here.
;might not be necessary, really
RestoreKeyboard:
 ld hl,0F50000h
 xor a		; Mode 0
 ld (hl),a
 inc l		; 0F50001h
 ld (hl),15	; Wait 15*256 APB cycles before scanning each row
 inc l		; 0F50002h
 xor a
 ld (hl),a
 inc l		; 0F50003h
 ld (hl),15	; Wait 15 APB cycles before each scan
 inc l		; 0F50004h
 ld a,8		; Number of rows to scan
 ld (hl),a
 inc l		; 0F50005h
 ld (hl),a	; Number of columns to scan
 ret

tSpriteID:
 .db 0
tSpritePAL:
 .db 0
 
linesClear:
 .db 0
pointsPerLine:
 .db 0,1,3,5,8
pointsPerMini:
 .db 1,2
pointsPerTLine:
 .db 4,8,12,16,24

CETrisSavVar:
 .db AppVarObj, "CETrisSV", 0
 
loadSaveVar:
 ld hl, CETrisSavVar
 call _Mov9ToOP1
 call _ChkFindSym
 jr c, createSave
 ld a,b
 or a
 ret z
 call _Arc_Unarc
 ld hl, CETrisSavVar
 call _Mov9ToOP1
 call _ChkFindSym
 ;de=data ptr
 ret
 
createSave:
 ld hl, CETrisSavVar
 call _Mov9ToOP1
 ld hl, savSize
 call _CreateAppVar
 ;de=data ptr
 push de
 ;needs to... put size at first (2 bytes!)
 ld hl,savSize
 ex de,hl
 ld (hl),de
 ;hl points to appvar begin
 inc hl
 inc hl
 ;hl points to actual data
 push hl
 pop de
 inc de ;de=hl+1
 ld bc,savSize - 1
 ld (hl),0
 ldir ;zero out all data
 ;put button info (after size)
 pop de
 push de
 inc de
 inc de ;de points to appvar begin (where button data goes)
 ld hl, defaultButtonData
 ld bc, defaultButtonDataSize
 ldir ;copy button data to save data
 ;default appvar name after buttons
 ;(not necessary now/maybe ever?)
 pop de
 ret

;note: this reads data from an already loaded save
;do not call before setting (saveDataPTR) to the data
loadSave:
 ld ix,(saveDataPTR) ;ix is ptr to save data
 lea hl, ix+savKeys ;get hl as savedata+keys
 ld de, buttonData 
 ld bc, defaultButtonDataSize
 ldir ;copy from ave
 
 lea hl, ix+savThemeSub
 ld a,(hl)
 ld (theme),a ;yes
 call applyTheme
 ret 
 
;assumes loaded and already in RAM
saveSave:
 ld ix,(saveDataPTR) ;ix is ptr to save data
 ld hl, buttonData 
 lea de, ix+savKeys ;get hl as savedata+keys
 ld bc, defaultButtonDataSize
 ldir ;copy from ave
 
 lea hl, ix+savThemeSub
 ld a,(theme)
 ld (hl),a
 
 ld hl, CETrisSavVar
 call _Mov9ToOP1
 call _Arc_Unarc
 ret
 
;copies data from SSSCopiedData to SSS
initData:
 ld hl,SSSCopiedData
 ld de,SSS
 ld bc,SSSCopiedDataEnd - SSSCopiedData
 ldir
 
 ld hl,PSS768CopiedData
 ld de,blockData
 ld bc,PSS768CopiedDataEnd - PSS768CopiedData
 ldir
 
 ;now read from SAVE
 ;ld hl,PSS1024CopiedData
 ;ld de,buttonData
 ;ld bc,PSS1024CopiedDataEnd - PSS1024CopiedData
 ;ldir
 
 ld a,30
 ld (lockDelay),a
 ret

;labels should be calculated relative to SSS
SSSCopiedData:
;these data references are pointers to pointers
;example: 
;ld ix,(drefSprite) ;ix points to spriteData
;ld ix,drefSprite ;ix points to the spriteData pointer
;load from (dref~~~~) to access relocated data
;example:
;dataReferences:
;.dl SSS + 128 ;sprite data
;...
;ld ix,(refSprite) ;ix now points to relocated sprite data
dataReferences:
.dl spriteData ;std sprites like blocks, field, etc
.dl fontData ;font data. 
.dl characterData ;character data
.dl bgData ;background tile data
.dl paletteData ;palette data
.dl menuObjData ;menu data
.dl pauseData
.dl SSSInfo ;ptr to item info for display in game
.dl gameOverData ;game over notif

drefSize = 3
drefOfs = dataReferences - SSSCopiedData + SSS

drefSprite = drefSize * 0 + drefOfs
drefFont = drefSize * 1 + drefOfs
drefCharacter = drefSize * 2 + drefOfs
drefBackground = drefSize * 3 + drefOfs
drefPalette	= drefSize * 4 + drefOfs
drefMenu = drefSize * 5 + drefOfs
drefPause = drefSize * 6 + drefOfs
drefIInfo = drefSize * 7 + drefOfs
drefGameOver = drefSize * 8 + drefOfs

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
.dl charInfo - itemsInfo + SSSInfo
.dl timerInfo - itemsInfo + SSSInfo

refSize = 3
refOfs = references - SSSCopiedData + SSS
refField = refSize * 0 + refOfs
refHold = refSize * 1 + refOfs
refLevel = refSize * 2 + refOfs
refScore = refSize * 3 + refOfs
refLines = refSize * 4 + refOfs
refChar = refSize * 5 + refOfs
refTimer = refSize * 6 + refOfs

;various equates for itemsInfo
iDataType 	= 0 ;type of data (use type~ to check)
iDataXL		= 1 ;x low byte
iDataXH		= 2 ;x high byte
iDataY		= 3 ;y
iDataA		= 4 ;a (used as color/palette for most)
iDataPTRL	= 5 ;ptr offset from SSS (or extra data)
iDataPTRH	= 6 ;sorry! SSS data must be <65536 bytes
iDataW		= 7 ;extra data (width, # digits)
iDataH		= 8 ;extra data 2 (height)
iDataSize	= 9 ;size of data struct

typeTetris=0
typeString=1
typeNumber=2
typeSprite=3
typeHold=4
typePreview=5
typeBox=6
typeMenu=7
typeSprite8bpp=8
typeSprite4bpp=9
typeSprite2bpp=10
typeSprite1bpp=11
typeNumber8=12
typeList=13

boxColor = 14
boxColor2= 15
textColor= 35

itemsInfo:
.db 9 ;number of items
fieldInfo:
.db typeTetris
.dw 0 ;x
.db 0 ;y
.db 0 ;a (does nothing?)
.dw 0 ;uses refSprite data ptr
.db 10, 20 ;width, height
holdInfo:
.db typeHold
.dw 132
.db 160
.db 0
.dw 0 ;uses refSprite data ptr
.db 4, 4 ;width, height
;BACKGROUND BOX INFO
.db typeBox
.dw 152 ;x
.db 16 ;y
.db boxColor ;color of main
.dw 128 ;width
.db boxColor2, 80 ;bordercolor, height
;SCORE TEXT ETC.
.db typeMenu
.dw 160
.db 24
.db textColor
.dw gameText - SSSCopiedData
.db 7, 0 ;cursorid doesn't matter cause not a active menu
scoreInfo:
.db typeNumber
.dw 208
.db 24
.db textColor
.dw score - PSS ;variables are saved in PSS
.db 8, boxColor ; size of number, bgcolor
levelInfo:
.db typeNumber
.dw 208
.db 48
.db textColor
.dw level - PSS
.db 3, boxColor ;size number, bg color
linesInfo:
.db typeNumber
.dw 208
.db 56
.db textColor
.dw lines - PSS
.db 4, boxcolor ;number of digits, bgcolor
timerInfo:
.db typeNumber
.dw 208
.db 32
.db textColor
.dw globalTimer - PSS
.db 8, boxcolor
highscoreInfo:
.db typeNumber
.dw 208
.db 72
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
 
SSSInfo = itemsInfo - SSSCopiedData + SSS

menuInfo = menuObjData - SSSCopiedData

menuObjData:
 .db 3
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
 .dw menuText - SSSCopiedData
 .db 3, 2 ;# items, cursorID within menuObjData
 ;cursor
 .db typeString
 .dw 0
 .db 8
 .db textColor
 .dw cursorString - SSSCopiedData
 .db 0, 0
 
cursorString:
 .db "*",0
 
menuText:
 .db "START",0
optionsText:
 .db "OPTIONS",0
 .db "EXIT",0
menuJumps:
 jp mainMenu ;first item is prev menu, if exists
 jp setupGame
 jp gotoOptions
 jp exit
 
setupGame:
 ld ix, startMenuData
 jp activeMenu
 
gotoOptions:
 ld ix, optionsMenuData
 jp activeMenu
 
startMenuData:
 .db 5
  ;background
 .db typeBox
 .dw 0 ;x
 .db 0 ;y
 .db 2 ;color
 .dw 200 ;width
 .db 1, 40 ;bordercolor, height
 ;menu text
 .db typeMenu
 .dw 8
 .db 8
 .db textColor
 .dw startMenuText - SSSCopiedData
 .db 3, 2 ;# items, cursorID within menuObjData
 ;cursor
 .db typeString
 .dw 0
 .db 8
 .db textColor
 .dw cursorString - SSSCopiedData
 .db 0, 0
startMenuSelectMode:
 .db typeString
 .dw 48
 .db 8
 .db textColor
 .dw modeText - SSSCopiedData
 .db 0, 2 ;bg color
startMenuSelectLev:
 .db typeNumber
 .dw 56
 .db 16
 .db textColor
 .dw level - PSS
 .db 3, 2 ;digits, bgcolor
 
startMenuText:
 .db "Mode:",0
 .db "Level:",0
 .db "BEGIN",0
startMenuJumps:
 jp mainMenu ;example of prev menu jump
 jp selectMode
 jp selectLev
 jp initGame ;starts game
 
selectLev:
 ld ix, startMenuSelectLev
 ld a, 20
 call setNumber
 
 ;after setting number,
 ;return to main start menu.
 ld ix, startMenuData
 jp activeMenu
 
selectMode:
 ld ix, selectModeMenu
 jp activeMenu
 
selectModeMenu:
 .db 3
 ;background
 .db typeBox
 .dw 0 ;x
 .db 0 ;y
 .db 9 ;color
 .dw 160 ;width
 .db 10, 128 ;bordercolor, height
 ;menu text
 .db typeMenu
 .dw 8
 .db 8
 .db textColor
 .dw modeText - SSSCopiedData
 .db 14, 2 ;# items, cursorID within menuObjData
 ;cursor
 .db typeString
 .dw 0
 .db 8
 .db textColor
 .dw cursorString - SSSCopiedData
 .db 0, 0

modeText:
.db "MARATHON-150",0
.db "MARATHON-200",0
.db "MARATHON-ENDLESS",0
.db "RETRO-150",0
.db "RETRO-200",0
.db "RETRO-ENDLESS",0
.db "LINE RACE-20",0 ;diff from marathon: hs based on time, not points
.db "LINE RACE-40",0
.db "DIG-5",0
.db "DIG-10",0
.db "DIG-15",0
.db "EXCAVATE-10-LIGHT",0
.db "EXCAVATE-10-MEDIUM",0
.db "EXCAVATE-10-DENSE",0
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
 ld a,2 << 5 | 10
 jr setExcavate
 ld a,5 << 5 | 10
 jr setExcavate
 ld a,7 << 5 | 10
 jr setExcavate

setMarathon:
 ld ix,rules
 ld (ix+rfBasic), rMarathon
 ld (ix+rfExtra), rNull ;no extra rules
 ld (ix+rfWin), rLines ;win on line-clears
 ld (ix+rfScore), rScore
 jr setLines
 
setRetro:
 ld ix,rules
 ld (ix+rfBasic), rRetro
 ld (ix+rfExtra), rNull ;no extra rules
 ld (ix+rfWin), rLines ;win on line-clears
 ld (ix+rfScore), rScore
 jr setLines

setLineRace:
 ld ix,rules
 ld (ix+rfBasic), rMarathon
 ld (ix+rfExtra), rNull ;no extra rules
 ld (ix+rfWin), rLines ;win on line-clears
 ld (ix+rfScore), rTime
 jr setLines
 
setDig:
 ld ix,rules
 ld (ix+rfBasic), rMarathon
 ld (ix+rfExtra), rGenerated ;no extra rules
 ld (ix+rfWin), rRow0 ;win on line-clears
 ld (ix+rfScore), rTime
 jr setGeneration

setExcavate: 
 ld ix,rules
 ld (ix+rfBasic), rMarathon
 ld (ix+rfExtra), rGenerated ;no extra rules
 ld (ix+rfWin), rRow0 ;win on line-clears
 ld (ix+rfScore), rTime
 jr setGenGGD
 
setLines: 
 cp 0
 jr z, setNoWin
 ld (ix+rLCW),a

 ;return to setup menu
slToMenu:
 ;replaces selected mode string
 call getStringPTRSelection
 ld (startMenuSelectMode + iDataPTRL),hl
 
 ld ix, rules
 ld a,(menuSelection)
 ld (ix+rMode),a
 ld ix,(saveDataPTR)
 ld de,0
 add a,a
 add a,a
 ld e,a
 add ix,de ;add 4
 add ix,de ;add 8
 ld hl,(ix+savHighScore) ;hl=score from save
 ld (highscore),hl
 
 ld ix, startMenuData 
 jp activeMenu
 
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
 .dw optionsText - SSSCopiedData
 .db 0, 0
;menu
 .db typeMenu
 .dw 8
 .db 16
 .db textColor
 .dw optionsMenuText - SSSCopiedData
 .db 2, 3
;curosr
 .db typeString
 .dw 0
 .db 16
 .db textColor
 .dw cursorString - SSSCopiedData
 .db 0, 0
;theme
themeSelection:
 .db typeNumber
 .dw 56
 .db 16
 .db textColor
 .dw theme - PSS
 .db 3, 19
 
optionsMenuText:
 .db "THEME:",0
 .db "CONTROLS",0
optionJumps:
 jp mainMenu
 jp setTheme
 jp controlMenu
 
controlMenu:
 ld ix,controlMenuData
 jp activeMenu
 
setTheme:
 ld ix,themeSelection
 ld a,4
 call setNumber
 
 call applyTheme
 
 ld ix, optionsMenuData
 jp activeMenu ;back to options menu
 
controlMenuData:
 .db 5
;background
 .db typeBox
 .dw 0
 .db 0
 .db 22
 .dw 132
 .db 23, 32
;menu
 .db typeMenu
 .dw 8
 .db 8
 .db textColor
 .dw controlMenuText - SSSCopiedData
 .db 2, 2
;cursor
 .db typeString
 .dw 0
 .db 8
 .db textColor
 .dw cursorString - SSSCopiedData
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
 
controlJumps:
 jp gotoOptions
 jp setButtonInfoLR
 jp setButtonInfoDrop
 
setButtonInfoLR:
 ld ix, keyRepeatLR
 ld a,255
 call setNumber
 
 ;affect both LEFT and RIGHT.
 ld a,(numberSelection)
 ld hl,buttonRight+buttonTimeRepeat
 ld (hl),a
 
 jp controlMenu
 
setButtonInfoDrop:
 ld ix, keyRepeatDrop
 ld a,255
 call setNumber

 jp controlMenu
 
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
 .dw pauseText - SSSCopiedData
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
 .dw gameOverText - SSSCopiedData
 .db 2, 2
;numbers etc
 
gameOverText:
 .db "GAME",0
 .db "OVER",0

;format: x ofs, y ofs, spriteID, palette
spriteID = 8
spritePAL= 9
;I piece
PSS768CopiedData:
 .db -1, 0
 .db  0, 0
 .db  1, 0
 .db  2, 0
;L piece
 .db -1,-1
 .db -1, 0
 .db  0, 0
 .db  1, 0
;J piece
 .db -1, 0
 .db  0, 0
 .db  1, 0
 .db  1,-1
;O piece
 .db  0,-1
 .db  1,-1
 .db  1, 0
 .db  0, 0
;S piece
 .db -1, 0
 .db  0, 0
 .db  0,-1
 .db  1,-1
;T piece
 .db  0, 0
 .db -1, 0
 .db  0,-1
 .db  1, 0
;Z piece
 .db -1,-1
 .db  0,-1
 .db  0, 0
 .db  1, 0
PSS768CopiedDataEnd:

blockGraphicData:
 .db  0, 4
 .db  0, 8
 .db  0, 12
 .db  0, 16
 .db  0, 20
 .db  0, 24
 .db  0, 28
 .db  0, 31
 
;wall kicks and rotation data
kicksNormal:
 .db  0, 0,  0, 0,  0, 0,  0, 0,  0, 0  
 .db  0, 0,  1, 0,  1, 1,  0,-2,  1,-2
 .db  0, 0,  0, 0,  0, 0,  0, 0,  0, 0
 .db  0, 0, -1, 0, -1, 1,  0,-2, -1,-2
 .db  0, 0,  0, 0,  0, 0,  0, 0,  0, 0  ;cloned so underflow isn't an issue
 
kicksI:
 .db  0, 0, -1, 0,  2, 0, -1, 0,  2, 0
 .db -1, 0,  0, 0,  0, 0,  0,-1,  0, 2
 .db -1,-1,  1,-1, -2,-1,  1, 0, -2, 0
 .db  0,-1,  0,-1,  0,-1,  0, 1,  0,-2
 .db  0, 0, -1, 0,  2, 0, -1, 0,  2, 0 ;same
 
kicksO:
 .db  0, 0
 .db  0, 1
 .db -1, 1
 .db -1, 0
 .db  0, 0
 
sp1bpp = 0
sp2bpp = 1
sp4bpp = 2
sp8bpp = 3
 
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

paletteData:
.dw $7fff, $1CE7, $3def, $0000
;dw $00ff, $0ff0, $ff00, $f00f
.dw $7fff, $4f7d, $029d, $1d39
.dw $7fff, $3a57, $1d39, $0000
.dw $7fff, $7f21, $7de4, $4402
.dw $7fff, $7fff, $7fc0, $7f21
.dw $7fff, $5b83, $12c9, $3def
.dw $7fff, $66fc, $5134, $0000
.dw $7fff, $7eb9, $7464, $4402
.dw $7fff, $1CE7, $3def, $0000
paletteDataEnd:
SSSCopiedDataEnd: