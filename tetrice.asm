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

blockDataSize = 10

;current status bits
csLockedBit = 0    ;locked block bit
csNewBlockBit = 1  ;prev. frame requests new block
csClearLineBit = 2 ;if block cleared a line

holdT= PSS + 256
;current mino data
curData = PSS + 266
curX = curData + 0
curY = curData + 1
curR = curData + 2
curT = curData + 3
curStatus = curData + 4
curBlock = curData + 5 ;size: 10
curDataSize = curBlock + blockDataSize - curX

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
timerT = PSS + 332
lockTimer = PSS + 336


rules = PSS + 512
rbitGameCont = 0
rbitGameEndless = 1
rbitSRSEnabled = 2
rbitPreviewEnabled = 3
rbitHoldEnabled = 4
rbitHardDropEnabled =5

;graphical and data resources
;note: info must be RELOCATED to this location.
SSS = saveSScreen

;bit set/reset when an object must be updated
;max redraw is 3 times
redrawObjBit = 7

;main program
main:
 ld (preserveSP),sp
 call initLCD
 call initData
 call mainMenu
 
exit:
 call resetLCD
 call restoreKeyboard
 
 ld sp,(preserveSP)
 ret

preserveSP:
.dl 0

mainMenu:
 ld ix,(drefMenu)
 call activeMenu
 ret
 
initGame:
 ld a,1
 ld (level),a
 ld a,0
 ld (lines),a
 
 ld a,7
 ld (holdT),a
 
 ld ix,rules
 set rbitGameCont,(ix+0)        ;game is going
 set rbitSRSEnabled,(ix+0)      ;use srs
 set rbitHoldEnabled,(ix+0)     ;use hold
 set rbitHardDropEnabled,(ix+0) ;allow hard drop
 
 ;clear field
 ld hl,field
 ld de,field+1
 ld bc,255
 ld (hl),NULL_BLOCK
 ldir
 
 call initBag
 call newBlock
 
 ;first draw to set up screen
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
 ld a,(keys+0)
 bit kbitDel, a
 ret nz
 
 call shiftOldData
 ;part of update code, essentially
 ;might move to update sometime
 call userUpdate
 call update
 call drawGame
 
 ld ix,rules
 bit rBitGameCont, (ix+0)
 jr nz, game ;jump if nz: bit is 1, game is going
 ret

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
 ld a,(keys+3)
 bit kbitLeft, a
 call nz, checkMoveLeft
 
 ld a,(keys+3)
 bit kbitRight, a
 call nz, checkMoveRight
 
 ld a,(keys+0)
 bit kbit2nd, a
 call nz, checkRotationLeft
 
 ld a,(keys+1)
 bit kbitAlpha, a
 call nz, checkRotationRight
 
 ld a,(keys+2)
 bit kbitGraphVar, a
 call nz, hold
 
 ld a,(keys+0)
 bit kbitMode, a
 call nz, pause
 ret
 
pause:
 halt
 call scanKeys
 
 ld a,(keys)
 bit kbitMode, a
 jr nz, pause

pauseLoop:
 halt
 call scanKeys

 ld a,(keys)
 bit kbitMode, a
 ret nz
 bit kbitDel, a
 ret nz
 
 call swapVRamPTR
 jr pauseLoop
 
 ret

pauseColor:
.db 0
pauseText1:
.db "~GAME~",0
pauseText2:
.db "PAUSED",0
 
keys:
.db 0,0,0,0
 
hold:
 ld ix,rules
 bit rbitHoldEnabled,(ix+0)
 ret z ;hold is disabled

 ld a,(holdT)
 cp 7
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

 ld a,(keys+3)
 bit kbitDown,a
 jr nz, userdrop
 ld a,(keys+3)
 bit kbitUp, a
 jr nz, harddrop
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
 jr dropReturn
 ret
 
userDrop:
 ld a,(lockTimer)
 cp LOCK_DISABLE
 jr nz, drop
 ld hl,(score)
 inc hl
 ld (score),hl
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
 
 ;call checkBlockDownOK
 ;cp 0
 ;jr z,gameEnd
 ;if block check fails immediately, it's a top out
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
 
 ;makeshift level calculation until it is more formalized
 ld hl,(lines)
 ld a,10
 call _DivHLbyA
 inc hl
 ld (level),hl
noLinesClear:
;all of the above do not update when not cleared.
 ret
 
clearLine:
 ;hl=end of cleared line
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
 add a,d
 add a,a
 ld de,0
 ld e,a
 add ix, de
 ld a,(ix+spritePAL)
 ld a,(curT) ;ensure copied blockdata is CORRECT?
 call copyBlockData
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
 add a,d
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
 ld a,10
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
 ld d,a ;save d for later
 ld hl, blockData
 add a,a ;x2
 add a,a ;x4
 add a,d ;x5
 add a,a ;x10
 ld de,0
 ld e,a ;de = offset from blockData
 add hl,de
 ;hl = pointer to blockdata
 ld de, curBlock
 ld bc, blockDataSize
 ldir ;copied to current block mem
 ret
 
;initialize 8bpp mode and palette
initLCD:
 call _RunIndicOff

 ld hl, PSS
 ld de, PSS + 1
 ld bc, 512
 ld (hl),0
 ldir
 
 ld a, lcdBpp8
 ld ($e30018),a 
 ld hl, vRam
 ld de, vRam+1
 ld bc, vRamEnd - vRam
 ld (hl),0
 ldir
 
;intiialize palette from data
 ld hl, paletteData
 ld de, $e30200
 ld bc, paletteDataEnd - paletteData
 ldir  
 ret

;return lcd to normal state (bpp16)
resetLCD:
;set default vram ptr
 ld hl,vRam
 ld (mpLcdUpbase),hl
 
 ld a, lcdBpp16
 ld ($e30018), a
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
 ld de, vRamSplit ;to second half of vRam, for double-buffering
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
 
;inputs: 
;ix = spritedataptr
;de = x
;l = y
;b = sizey
;c = sizex
;a = spritePalette
drawSprite:
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
 ld de, (vramOffPtr) ;to second half of vRam, for double-buffering
 add hl,de ;320Y+x+vRam
;hl=vramptr ix=data b=sizeY c=sizeX
putSpriteYLoop:
 push bc
 ld b,c
 push hl ;save ptr to graph
putSpriteXLoop:
 push af
 ld a,(ix+0)
 cp 0 ;check if no color
 jp z,transPixel
 pop af
 push af
 add a,(ix+0) ;add color to palette ofs
 ld (hl),a
transPixel:
 pop af
 inc hl
 inc ix
 djnz putSpriteXLoop
 pop hl
 ld de,320
 add hl,de ;move down 1 y unit
 pop bc
 djnz putSpriteYLoop
 ret

drawHeldMino:
 ld a,(holdT)
 ;ld d,a ;save d for later
 ;push hl
 ;ld hl, blockData
 ;add a,a ;x2
 ;add a,a ;x4
 ;add a,d ;x5
 ;add a,a ;x10
 ;ld de,0
 ;ld e,a ;de = offset from blockData
 ;add hl, de ;block data ptr
 ;push hl
 ;ld de,spriteID
 ;add hl,de
 ;ld a, (hl)
 ;ld (tSpriteID),a
 ;inc hl ;block data + id + pal
 ;ld a, (hl)
 ;ld (tSpritePAL),a
 ;ld b,4
 ;pop de ;bdptr
 ;pop hl ;coords
 ;;jp drawMino ;not needed because structure
 
;inputs:
; h=x (in grid tiles)
; l=y (in grid tiles)
; a=mino type
drawMino:
 ;ld d,a ;save d for later
 push hl
 ld hl, curBlock
 ;add a,a ;x2
 ;add a,a ;x4
 ;add a,d ;x5
 ;add a,a ;x10
 ;ld de,0
 ;ld e,a ;de = offset from blockData
 ;add hl, de ;block data ptr
 push hl ;stack:  bdptr-coords
 ld de, spriteID
 add hl, de ;block data + id
 ld a, (hl)
 ld (tSpriteID),a
 inc hl ;block data + id + pal
 ld a, (hl)
 ld (tSpritePAL),a
 ld b,4
 pop de ;bdptr
 pop hl ;coords
drawAllBlocks:
 push bc
 push hl ;coords
 ld a,(de)
 add a,h
 ld h,a
 inc de
 ld a,(de)
 add a,l
 ld l,a
 inc de
 push de
 call drawOneBlock
 pop de
 pop hl ;coords
 pop bc
 djnz drawAllBlocks
 ret
 
;inputs:
;h=x+ofsx
;l=y+ofsy
;(tSpritePal), (tSpriteID)
drawOneBlock:
 ld a,l
 cp 21
 ret nc ;do not draw OOB
 ld a,h
 cp 26
 ret nc ;still no OOB
 ;push hl ;save xy
 ld e,h ;save e=x
 ld h,12
 mlt hl ;hl = 12*y implies l=12*y since y<20
 ld d,12
 mlt de ;de = 12*(x + xOfs)
 
 ;starting here:
 ;skips bounds check 
 ;accepts (DE, L) as coordinates
 ;still needs tSpriteID and tSpritePAL
drawOneBlockNoGrid:
 ld a, (tSpriteID)
 ld b,144
 ld c,a ;sprite size * a
 mlt bc
 ld ix, (drefSprite)
 add ix,bc ;spriteData+64*spriteID
 
 ld a, (tSpritePAL)
 ld b,12
 ld c,b
 call drawSprite
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
 call swapVRamPTR
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
 
drawGameOld2:
 ld ix,(drefIInfo)
 call drawObjectsNoReset
 
 ld hl, curStatus
 bit csClearLineBit, (hl)
 jr nz, drawSkip
 
 ld hl, curStatus + midOfs
 bit csClearLineBit, (hl)
 jr nz, clearSkip
 
 call clearOld

clearSkip:
 call drawCurrent
 
drawSkip:
 call swapVRamPTR
 ;draw again
 ld ix,(drefIInfo)
 call drawObjects ;will always check for draw twice, incase needed
 
 ld hl, curStatus
 bit csClearLineBit, (hl)
 ret nz
 ld hl, curStatus + midOFs
 bit csClearLineBit, (hl)
 ret nz
 
 call drawMidAsCurrent
 ret

clearOld:
 ld hl, curStatus + oldOfs
 bit csClearLineBit, (hl)
 ret nz ;don't erase if line was cleared (not drawn)
 
 ld hl, curStatus + oldOfs
 bit csLockedBit, (hl)
 ret nz ;don't erase if locked (should stay)
 
 ld hl, oldData
 ld ix,(refField)
 call NullPTRMino
 ret 
 
drawCurrent:
 ld ix,(refField)
 call drawCurrentMino
 ret

drawMidAsCurrent:
 ;new current frame is mid because it locked prev. frame and needs to draw to both buffers
 ld hl, curStatus
 bit csLockedBit, (hl)
 ret z ;if not locked, do not draw this frame.

 ld ix,(refField)
 ld hl,curData
 call drawMinoFromPTR
 
 ld hl, midData
 ld de, midData+1
 ld bc, curDataSize * 2 - 1
 ld (hl),0
 ldir
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
 ret ;this one is just aesthetic
 
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
 add a,a ;4
 add a,d ;5
 add a,a ;10
 ld de,0
 ld e,a
 ld hl,blockData + spriteID
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
 ld d,a ;save d for later
 ld hl, blockData
 add a,a ;x2
 add a,a ;x4
 add a,d ;x5
 add a,a ;x10
 ld de,0
 ld e,a ;de = offset from blockData
 add hl, de ;block data ptr
 push hl
 ld de,spriteID
 add hl,de
 ld a, (hl)
 ld (tSpriteID),a
 inc hl ;block data + id + pal
 ld a, (hl)
 ld (tSpritePAL),a

 pop de ;bdptr
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
 ld bc,0
 ld b,a ;256a = bc
 srl b
 rr c ;128a = bc
 srl b 
 rr c ;64a = bc
 ld ix,(drefFont)
 add ix,bc ;fontData + 64a (character)
 ld bc,-2048 ;account for missing ctrl characters
 add ix,bc ;fontData + 64(a - 32) ;ascii adjust
 
 ld b,8
 ld c,b ;8x8 font

 ld a,(drawTextBG)
 cp 0
 jr z,textTransBG
 call clearSprite
textTransBG:
 ld a,(drawTextColor)
 call drawSprite
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
draw24Saved:
.dl 0
d24NColor:
.db 0, 0
draw24BitNumber:
 push ix
 ld hl,0
 ld h,(ix+iDataPTRH)
 ld l,(ix+iDataPTRL)
 ld bc,PSS
 add hl,bc ;ptr to number
 
 ld a,(hl)
 ld (draw24Saved),a
 inc hl
 ld a,(hl)
 ld (draw24Saved+1),a
 inc hl
 ld a,(hl)
 ld (draw24Saved+2),a
 
 ld hl,(draw24Saved) ;this is the number
 
 ld d,(ix+iDataXL)
 ld e,(ix+iDataY)
 ld b,(ix+iDataW)
 ld a,(ix+iDataA)
 ld (d24NColor),a
 ld a,(ix+iDataH) ;bg color
 ld (d24NColor+1),a
 ld (draw24Saved),de
d24bnLoop:
 push bc
 ld a,10
 call _DivHLbyA ;apparently a is remainer here
 push hl ;save score for later
 ld de,(drefFont)
 ld hl,numbers - fontData ;offset to numbers
 add hl,de ;fontdata + offset to numbers
 push hl
 pop de ;de=fontData
 ld hl,0
 ld l,a
 add hl,hl
 add hl,hl
 add hl,hl
 add hl,hl
 add hl,hl
 add hl,hl
 add hl,de ;sprite data is here
 push hl
 pop ix
 ld de,(draw24Saved)
 ld l,e ;l=y
 push hl
 ld a,d 
 ld hl,0
 ld l,a ;de=x
 ld a,b ;offset per character
 add a,a
 add a,a
 add a,a ;8*a
 ld bc,0
 ld c,a
 add hl,bc ;add offset to x
 push hl
 pop de ;de=x
 pop hl ;restore l=y
 ld bc, $0808
 ld a,(d24NColor+1)
 cp 0
 jr z,d24TransBG
 call clearSprite
d24TransBG:
 ld a,(d24NColor)
 call drawSprite
 pop hl
 pop bc
 djnz d24bnLoop
 pop ix ;restore data ptr
 ret

;ix = input
drawSpriteObject:
 push ix
 ld hl,0
 ld h,(ix+iDataPTRH)
 ld l,(ix+iDataPTRL)
 ld bc,SSS
 add hl,bc ;ptr to number
 push hl ;ptr to sprite data
 
 ld de,0
 ld d,(ix+iDataXH)
 ld e,(ix+iDataXL)
 
 ld bc,0
 ld b,(ix+iDataH)
 ld c,(ix+iDataW)
 ld a,(ix+iDataA)
 
 ld hl,0
 ld l,(ix+iDataY)
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
 
menuWaitNoInput:
 call scanKeys
 
 ld a,(keys)
 bit kbitDel,a
 jr nz,menuWaitNoInput ;wait for no selection
 jr menuDraw
  
menuLoop:
 call scanKeys
 
 ld a,(keys)
 bit kbit2nd,a
 jr nz,menuSelect
 
 ld a,(keys+3)
 bit kbitUp,a
 jr nz,menuUp
 
 ld a,(keys+3)
 bit kbitDown,a
 jr nz,menuDown
 
 ld a,(keys)
 bit kbitDel,a
 jp nz,exit
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
 call drawObjects
 call swapVRamPTR
 
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
 ld b,a
 ld ix,(menuPTR)
 ld a,(ix+iDataW)
 dec a
 dec a
 cp b
 jr c, menuDraw ;don't go past end of list
 inc a
 ld (menuSelection),a
 jr menuDraw
 
menuSelect:
 ld de,0
 ld a,(menuSelection)
 ld e,a
 
smcLoadJumpTable:
 ld hl,$000000 ;this will be replaced
 add hl,de
 add hl,de
 add hl,de
 add hl,de
 
 jp (hl)
 ret ;ha
 
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
;I no longer know how this works, it's a copy of a copy of a z80 version I wrote.
;The idea is something to do with right shifting
;and XORing with the previously shifted bit to get a long cycle.
;Just read the wikipedia page.
rand:
 ld de,(randseed)
 ld a,(randinput)
 cp 0
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
 ld a,%10110100 ;this is the ideal set of bits to invert for a maximal cycle, apparently.
 xor d
 ld d,a
 ret

;old-new random
random:
 ld hl,randsav
 ld a,r
 and $03
 ld b,a ;id
 inc b
randIncSav:
 inc hl
 djnz randIncSav
 dec hl
 
 ;hl is one of four random slots
 ld a,(hl)
 inc a
 ld (hl),a
 cp RANDOM_NULL
 ret nz
 ld a,0
 ld (hl),a
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

 ; Read data registers here as needed
 ld a,(kbdG1)
 ld (keys),a
 ld a,(kbdG2)
 ld (keys+1),a
 ld a,(kbdG3)
 ld (keys+2),a
 ld a,(kbdG7)
 ld (keys+3),a
 
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
 .db 1,2,4 
pointsPerTLine:
 .db 4,8,12,16,24

;copies data from SSSCopiedData to SSS
initData:
 ld hl,SSSCopiedData
 ld de,SSS
 ld bc,SSSCopiedDataEnd - SSSCopiedData
 ldir
 ret
 
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
.dl levelInfo + iDataSize - itemsInfo + SSSInfo
.dl scoreInfo + iDataSize - itemsInfo + SSSInfo
.dl linesInfo + iDataSize - itemsInfo + SSSInfo
.dl charInfo - itemsInfo + SSSInfo

refSize = 3
refOfs = references - SSSCopiedData + SSS
refField = refSize * 0 + refOfs
refHold = refSize * 1 + refOfs
refLevel = refSize * 2 + refOfs
refScore = refSize * 3 + refOfs
refLines = refSize * 4 + refOfs
refChar = refSize * 5 + refOfs

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

boxColor = 14

itemsInfo:
.db 10 ;number of items
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
.dw 96 ;width
.db 15,72 ;bordercolor, height
scoreInfo:
.db typeString
.dw 160
.db 24
.db 32
.dw scoreText - SSSCopiedData ;ptr to string data
.db 0, boxColor ;unused, bgcolor
;SCORE NUMBER
.db typeNumber
.dw 160
.db 32
.db 33
.dw score - PSS ;variables are saved in PSS, data is saved in SSS
.db 8, boxColor ; size of number, bgcolor
levelInfo:
.db typeString
.dw 160
.db 48
.db 32
.dw levelText - SSSCopiedData
.db typeString, boxColor ;old, bg color
;LEVEL NUMBER
.db typeNumber
.dw 160
.db 56
.db 33
.dw level - PSS
.db 3, boxColor ;size number, bg color
linesInfo:
.db typeString
.dw 160
.db 72
.db 32
.dw linesText - SSSCopiedData
.db typeString, boxColor ;old, bgcolor
;LINES NUMBER
.db typeNumber
.dw 160
.db 80
.db 33
.dw lines - PSS
.db 4, boxcolor ;number of digits, bgcolor
;DEBUG TEXT
.db typeNumber
.dw 204
.db 80
.db 32
.dw timerT - PSS
.db 3, boxcolor

scoreText:
 .db "Score:",0
levelText:
 .db "Level:",0
linesText:
 .db "Lines:",0

SSSInfo = itemsInfo - SSSCopiedData + SSS
 
menuInfo = menuObjData - SSSCopiedData

menuObjData:
 .db 3
 ;background
 .db typeBox
 .dw 0 ;x
 .db 0 ;y
 .db 5 ;color
 .dw 64 ;width
 .db 6, 32 ;bordercolor, height
 ;menu text
 .db typeMenu
 .dw 8
 .db 8
 .db 32
 .dw menuText - SSSCopiedData
 .db 2, 2 ;# items, cursorID within menuObjData
 ;cursor
 .db typeSprite
 .dw 0
 .db 8 
 .db 32
 .dw fontData - SSSCopiedData + 640
 .db 8, 8
 
menuText:
 .db "START",0
 .db "EXIT",0
menuJumps:
 jp initGame
 jp exit
 
;format: x ofs, y ofs, spriteID, palette
spriteID = 8
spritePAL= 9
;I piece
blockData:
 .db -1, 0
 .db  0, 0
 .db  1, 0
 .db  2, 0
 .db  0, 4
;L piece
 .db -1,-1
 .db -1, 0
 .db  0, 0
 .db  1, 0
 .db  1, 8
;J piece
 .db -1, 0
 .db  0, 0
 .db  1, 0
 .db  1,-1
 .db  2, 12
;O piece
 .db  0,-1
 .db  1,-1
 .db  1, 0
 .db  0, 0
 .db  3, 16
;S piece
 .db -1, 0
 .db  0, 0
 .db  0,-1
 .db  1,-1
 .db  4, 20
;T piece
 .db  0, 0
 .db -1, 0
 .db  0,-1
 .db  1, 0
 .db  5, 24
;Z piece
 .db -1,-1
 .db  0,-1
 .db  0, 0
 .db  1, 0
 .db  6, 28

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
 
spriteData:
.db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2
.db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 3
.db 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3
.db 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3
.db 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3
.db 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3
.db 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3
.db 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3
.db 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3
.db 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3
.db 1, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3
.db 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3

.db 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1
.db 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2
.db 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 2
.db 0, 0, 0, 1, 1, 1, 1, 1, 1, 2, 2, 2
.db 0, 0, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2
.db 0, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2
.db 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2
.db 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3
.db 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3
.db 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3
.db 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3
.db 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3

.db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0
.db 0, 1, 0, 2, 2, 2, 2, 2, 2, 0, 1, 0
.db 0, 1, 0, 2, 0, 0, 0, 0, 2, 0, 1, 0
.db 0, 1, 0, 2, 0, 3, 3, 0, 2, 0, 1, 0
.db 0, 1, 0, 2, 0, 3, 3, 0, 2, 0, 1, 0
.db 0, 1, 0, 2, 0, 0, 0, 0, 2, 0, 1, 0
.db 0, 1, 0, 2, 2, 2, 2, 2, 2, 0, 1, 0
.db 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

.db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 0
.db 0, 1, 2, 3, 3, 3, 3, 3, 3, 2, 1, 0
.db 0, 1, 2, 3, 3, 3, 3, 3, 3, 2, 1, 0
.db 0, 1, 2, 3, 3, 3, 3, 3, 3, 2, 1, 0
.db 0, 1, 2, 3, 3, 3, 3, 3, 3, 2, 1, 0
.db 0, 1, 2, 3, 3, 3, 3, 3, 3, 2, 1, 0
.db 0, 1, 2, 3, 3, 3, 3, 3, 3, 2, 1, 0
.db 0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

.db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 0
.db 0, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 0
.db 0, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 2, 2, 1, 1, 1, 1, 2, 2, 1, 0
.db 0, 1, 2, 2, 2, 2, 2, 2, 2, 2, 1, 0
.db 0, 1, 1, 2, 2, 2, 2, 2, 2, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

.db 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2
.db 0, 0, 0, 0, 1, 1, 2, 2, 2, 2, 2, 2
.db 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 2, 2
.db 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 1
.db 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 1, 1
.db 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1
.db 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1
.db 2, 2, 2, 0, 0, 0, 0, 0, 0, 3, 3, 3
.db 2, 2, 2, 2, 0, 0, 3, 3, 3, 3, 3, 3
.db 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3
.db 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3
.db 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3

.db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
.db 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1
.db 1, 2, 2, 3, 3, 3, 3, 3, 3, 3, 2, 1
.db 1, 2, 3, 3, 3, 3, 3, 3, 3, 2, 2, 1
.db 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1
.db 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1

fontData:
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 1, 1, 0, 0
.db 0, 1, 1, 0, 1, 1, 0, 0
.db 0, 0, 1, 0, 0, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 1, 1, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 0, 1, 1, 0, 0
.db 0, 1, 1, 0, 1, 1, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 0, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 0, 1, 1, 0, 1, 0
.db 0, 0, 1, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 1, 0, 0
.db 0, 1, 0, 1, 1, 0, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 1, 1, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 1, 1, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 0, 0, 0
.db 0, 1, 1, 0, 1, 1, 0, 0
.db 0, 0, 1, 1, 1, 0, 0, 0
.db 0, 1, 1, 0, 1, 0, 1, 0
.db 0, 1, 1, 0, 1, 1, 0, 0
.db 0, 0, 1, 1, 1, 0, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 1, 1, 0, 0, 0, 0
.db 0, 0, 1, 1, 0, 0, 0, 0
.db 0, 0, 1, 1, 0, 0, 0, 0
.db 0, 0, 1, 1, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 1, 1, 0, 0
.db 0, 0, 0, 0, 1, 1, 0, 0
.db 0, 0, 0, 0, 1, 1, 0, 0
.db 0, 0, 0, 0, 1, 1, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 1, 1, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 1, 1, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
numbers:
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 1, 1, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 1, 1, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 1, 1, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 0, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 1, 1, 0, 0
.db 0, 0, 0, 0, 1, 1, 0, 0
.db 0, 0, 0, 0, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 0, 1, 1, 1, 1, 0
.db 0, 1, 1, 1, 1, 0, 0, 0
.db 0, 1, 1, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 1, 1, 0
.db 0, 0, 0, 1, 1, 1, 1, 0
.db 0, 1, 1, 1, 1, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 0, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 1, 1, 1, 0
.db 0, 1, 1, 0, 1, 0, 1, 0
.db 0, 1, 1, 0, 0, 1, 0, 0
.db 0, 0, 1, 1, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 1, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 1, 1, 0, 0
.db 0, 1, 1, 1, 1, 0, 0, 0
.db 0, 1, 1, 0, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 1, 0, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 0, 1, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 0, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 0, 1, 1, 1, 0, 0
.db 0, 0, 1, 1, 1, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 1, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 1, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 1, 1, 1, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 1, 1, 1, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 1, 1, 0, 0
.db 0, 1, 1, 1, 1, 0, 0, 0
.db 0, 1, 1, 0, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 1, 1, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 1, 1, 0
.db 0, 0, 1, 1, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 1, 1, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 1, 1, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 1, 1, 1, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 1, 1, 1, 0, 0, 0, 0
.db 0, 1, 1, 1, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 1, 1, 1, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 0, 0, 0, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 0, 0, 0, 1, 1, 1, 0
.db 0, 0, 0, 0, 1, 1, 1, 0
.db 0, 0, 0, 1, 1, 0, 0, 0
.db 0, 1, 1, 1, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 1, 1, 0, 0, 0, 0
.db 0, 1, 0, 1, 1, 0, 1, 0
.db 0, 0, 0, 0, 1, 1, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 0, 0, 0, 0
.db 0, 0, 1, 0, 0, 0, 0, 0
.db 0, 1, 1, 1, 1, 1, 1, 0
.db 0, 0, 1, 0, 0, 0, 0, 0
.db 0, 0, 0, 1, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
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
