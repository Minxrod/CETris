#include "includes\ti84pce.inc"

; Credits:
; Minxrod - Primary developer
; Michael - troubleshooting

 .assume ADL=1
 .org userMem-2
 .db tExtTok,tAsm84CeCmp

;program specfic constants:
LOCK_DISABLE = -1
vRamSplitSize = (vRamEnd - vRam) / 2 
vRamSplit = vRam + vRamSplitSize 

;variables and memory regions
PSS = plotSScreen
field = PSS
holdT= PSS + 256
curX = PSS + 266
curY = PSS + 267
curR = PSS + 268
curT = PSS + 269
timerT = PSS + 270 ;to 272
lockTimer = PSS + 273
curPAL = PSS + 274
curBAG = PSS + 275
curBlock = PSS + 276
score = PSS + 300 ;to 302: max score is 16777216
level = PSS + 304
lines = PSS + 308 

;main program
main:
 call initLCD
 call initGame
 
 call resetLCD
 ret
 
initGame:
 ld a,0
 ld (level),a
 ld a,7
 ld (holdT),a
 
 call initBag
 call newBlock
 
game:
 ld a,(kbdG7)
 bit kbitLeft, a
 call nz, checkMoveLeft
 ld a,(kbdG7)
 bit kbitRight, a
 call nz, checkMoveRight
 ld a,(kbdG1)
 bit kbit2nd, a
 call nz, checkRotationLeft
 ld a,(kbdG2)
 bit kbitAlpha, a
 call nz, checkRotationRight
 ld a,(kbdG3)
 bit kbitGraphVar, a
 call nz, hold
 ;check exit
 ld a,(kbdG1)
 bit kbitDel, a
 ret nz
 call update
 call draw
 jp game
 ret

hold:
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
 
update:
 ld hl,(timerT)
 inc hl
 ld (timerT),hl
 
 ;calculate gravity
 ld b,l ;current time
 ld a, (level)
 add a,a
 add a,a
 ld l, a ;l=2*level
 ld a, 60
 sub l ;60 - level
 cp b
 jr c, drop
 ld a,(lockTimer)
 cp -1
 jr z, skipLockCheck
 dec a
 ld (lockTimer),a
 cp 0
 jr z, lock
skipLockCheck:
 ld a,(kbdG7)
 bit kbitDown,a
 jr nz, userdrop
 ld a,(kbdG7)
 bit kbitUp, a
 jr nz, harddrop
 ret

hardDrop:
 ld a,(lockTimer)
 cp LOCK_DISABLE
 jr nz, hdrop ;lock not disabled
 ld hl,(score)
 inc hl
 inc hl
 ld (score),hl
 ld hl,0
 ld (timerT),hl
 call checkBlockDown
 jr hardDrop
hdrop:
 ld a,5
 ld (lockTimer),a ;lock enabled for 5 frames
 ret
 
userDrop:
 ld a,(lockTimer)
 cp -1
 jr nz, drop
 ld hl,(score)
 inc hl
 ld (score),hl
drop:
 ld hl,0
 ld (timerT),hl
 call checkBlockDown
 ret
 
lock:
 ld a,-1
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
 call checkBlock
 ld a,(curPAL)
 ld (hl),a
 pop hl
 inc hl
 inc hl
 pop bc
 djnz lockAllBlocks
 call checkLines ;only check line clears after a lock
 call newBlock
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
 ld a,0
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
 ld de,0
 ld e,a ;de is lines cleared
 ld hl,(lines)
 add hl,de
 ld (lines),hl
 
 ld hl,pointsPerLine
 add hl,de
 ld de,0
 ld a,(hl) ;score to add for lines cleared
 ld e,a
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
 ld (hl),0
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
 ld (curPAL),a
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
 ld (curPAL),a
 ld a,(curT) ;ensure copied blockdata is CORRECT?
 call copyBlockData
 ret
 
draw:
 ;ld hl, vRam
 ;ld de, vRam+1
 ;ld bc, vRamEnd - vRam
 ;ld (hl),0
 ;ldir
 ld b,20
drawBlockFieldY:
 push bc
 ld c,b
 ld b,10 ;(b,c) = (x,y)
drawBlockFieldX:
 push bc
 ld h,b
 dec h
 ld l,c
 dec l
 push hl
 call checkBlock
 ld a,(hl)
 ld (tSpritePAL),a
 pop hl ;use same coords
 call drawOneBlock
 pop bc
 djnz drawBlockFieldX
 pop bc
 djnz drawBlockFieldY
 
 ld a,(curX)
 ld h,a
 ld a,(curY)
 ld l,a
 ld a,(curT)
 call drawMino
 
 ;draw hold field
 ld b,4
drawHoldFieldY:
 push bc
 ld c,b
 ld b,4 ;(b,c) = (x,y)
drawHoldFieldX:
 push bc
 push hl
 ld h,b
 ld a,10
 add a,h
 ld h,a
 ld l,c
 ld a,14
 add a,l
 ld l,a
 ld a,0
 ld (tSpritePAL),a
 call drawOneBlock
 pop hl
 pop bc
 djnz drawHoldFieldX
 pop bc
 djnz drawHoldFieldY
 
 ld h,12
 ld l,17
 ld a,(holdT)
 call drawHeldMino
 
 call drawScore
 call copyBufferVRam ;double-buffering
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
 cp 0
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
 ld a, LOCK_DISABLE
 ld (lockTimer),a
 call checkBlockDownOK
blockTooFarLeft:
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
 cp 0
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
 
 ld a, -1
 ld (lockTimer),a
 call checkBlockDownOK
 
 ld a,(curR)
 dec a
 and $03
 ld (curR),a
 
 ld a,(rX)
 ld (curX),a
 ld a,(rY) ;save the location of the successful turn.
 ld (curY),a
 ret
noRotationLeft:
 pop hl ;get offset
 
 ld de,-10
 call calculateRXY ;rxy stored - hl is already at next ofs
 push hl ;at next offset already
 
 ld hl, curBlock
 ld b,4
 
 ld a,(rAttempt)
 inc a
 ld (rAttempt),a
 cp 5
 jp nz, checkBlocksRotateLeft

 pop hl ;unneeded.
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
 cp 0 ;check if no block
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
 
 ld a, -1
 ld (lockTimer),a
 call checkBlockDownOK
 
 ld a,(curR) ;increment angle
 inc a
 and $03 ;keep in bounds
 ld (curR),a
 
 ld a,(rX)
 ld (curX),a
 ld a,(rY) ;save the location of the successful turn.
 ld (curY),a
 ret
noRotationRight:
 pop hl ;get offset

 ld de,10
 call calculateRXY ;new rxy stored - hl is already at next ofs
 push hl ;at next offset already
 ;reinitialize loop
 ld hl, curBlock
 ld b,4
 
 ld a,(rAttempt)
 inc a
 ld (rAttempt),a
 
 cp 5 ;compare rAttempt: if 5 attempts have hit do not jump back
 jp nz, checkBlocksRotateRight
 
 pop hl ;unneeded.
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
 cp 0
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
 ld a, LOCK_DISABLE
 ld (lockTimer),a
 call checkBlockDownOK
blockTooFarRight:
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
 cp 0
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
 cp -1
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
 ld a, lcdBpp16
 ld ($e30018), a
 call _ClrLCDFull
 call _DrawStatusBar
 ret

;sprite routine draws to vram + (vRamEnd - vRam)/2. this copies from that address to vRam. 
copyBufferVRam:
 ld hl, vRamSplit
 ld de, vRam
 ld bc, vRamSplitSize
 ldir
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
 ld de, vRamSplit ;to second half of vRam, for double-buffering
 add hl,de ;320Y+x+vRam
;hl=vramptr ix=data b=sizeY c=sizeX
putSpriteYLoop:
 push bc
 ld b,c
 push hl ;save ptr to graph
putSpriteXLoop:
 push af
 add a,(ix+0)
 cp 0
 jp z,transPixel
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
 ld d,a ;save d for later
 push hl
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
 ld b,4
 pop de ;bdptr
 pop hl ;coords
 jp drawAllBlocks
 
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
;(tSpritePal)
drawOneBlock:
 ld a,l
 cp 21
 ret nc ;do not draw OOB
 ld a,h
 cp 26
 ret nc ;still no OOB
 push hl ;save xy
 ld e,h ;save e=x
 ld h,12
 mlt hl ;hl = 12*y implies l=12*y since y<20
 ld d,12
 mlt de ;de = 12*(x + xOfs)
 ld b,12
 ld c,b
 ld a, (tSpritePAL)
 ld ix, spriteData
 call drawSprite
 pop hl ;restore old coordinates
 ret
 
drawScore:
 ld a,0
 ld b,8
 ld c,48
 ld de, 160
 ld l,0
 ld ix,scoreSprite
 call drawSprite
 
 ld a,0
 ld b,8
 ld c,48
 ld de, 160
 ld l, 24
 ld ix,levelSprite
 call drawSprite
 
 ld a,0
 ld b,8
 ld c,48
 ld de, 160
 ld l, 48
 ld ix,linesSprite
 call drawSprite
 ;calculate score display.
 
 ld hl,(score)
 ld b, 8
 ld d, 160
 ld e, 8
 call draw24BitNumber
 
 ld hl,(level)
 ld b, 2
 ld d, 160
 ld e, 32
 call draw24BitNumber
 
 ld hl,(lines)
 ld b, 4
 ld d, 160
 ld e, 56
 call draw24BitNumber
 
 call drawDebugInfo
 ret

drawDebugInfo:
 ld hl,0
 ld a,(curR)
 ld l,a
 ld b,3
 ld d,160
 ld e,120
 call draw24BitNumber
 
 ld hl,0
 ld a,(curX)
 ld l,a
 ld b,3
 ld d,160
 ld e,128
 call draw24BitNumber
 
 ld hl,0
 ld a,(curY)
 ld l,a
 ld b,3
 ld d,160
 ld e,136
 call draw24BitNumber
 
 ld hl,0
 ld a,(rX)
 ld l,a
 ld b,3
 ld d,160
 ld e,152
 call draw24BitNumber
 
 ld hl,0
 ld a,(rY)
 ld l,a
 ld b,3
 ld d,160
 ld e,160
 call draw24BitNumber
 
 ld hl,0
 ld a,(rAttempt)
 ld l,a
 ld b,2
 ld d,160
 ld e,168
 call draw24BitNumber
 
 ld b,8 ;4 pairs of x,y
 ld hl,curBlock
 ld e,136 ;starting y coord
drawBlockCoords:
 push bc
 push hl
 
 ld a,(hl) ;load from block struct
 ld hl,0
 ld l,a
 ld b,3
 ld d,200
 push de
 call draw24BitNumber
 pop de
 ld a,8
 add a,e
 ld e,a ;add 8 to Y coords
 pop hl
 inc hl ;next item of blockdata
 pop bc
 djnz drawBlockCoords
 ret
 
;inputs: b=num digits
;hl = number
;d,e = x,y
draw24Saved:
 .db 0,0,0
draw24BitNumber:
 ld (draw24Saved),de
d24bnLoop:
 push bc
 ld a,10
 call _DivHLbyA ;apparently a is remainer here
 push hl ;save score for later
 ld de,numbers
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
 ld a,32
 call drawSprite
 pop hl
 pop bc
 djnz d24bnLoop
 ret
 
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
 
;format: x ofs, y ofs, spriteID, palette
spriteID = 8
spritePAL= 9
;I piece
blockDataSize = 10
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
 .db  0, 8
;J piece
 .db -1, 0
 .db  0, 0
 .db  1, 0
 .db  1,-1
 .db  0, 12
;O piece
 .db  0,-1
 .db  1,-1
 .db  1, 0
 .db  0, 0
 .db  0, 16
;S piece
 .db -1, 0
 .db  0, 0
 .db  0,-1
 .db  1,-1
 .db  0, 20
;T piece
 .db  0, 0
 .db -1, 0
 .db  0,-1
 .db  1, 0
 .db  0, 24
;Z piece
 .db -1,-1
 .db  0,-1
 .db  0, 0
 .db  1, 0
 .db  0, 28

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

scoreSprite:
.db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 3, 3, 3, 3, 3, 0, 0, 0, 3, 3, 3, 3, 0, 0, 0, 0, 3, 3, 3, 3, 0, 0, 0, 3, 3, 3, 3, 3, 0, 0, 0, 3, 3, 3, 3, 3, 3, 0, 0, 0, 3, 3, 0, 0, 0, 0
.db 0, 3, 3, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0
.db 0, 3, 3, 3, 3, 3, 0, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 3, 3, 3, 0, 0, 0, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 3, 3, 3, 3, 3, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0
.db 0, 3, 3, 3, 3, 3, 0, 0, 0, 0, 3, 3, 3, 3, 0, 0, 0, 0, 3, 3, 3, 3, 0, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 3, 3, 3, 3, 0, 0, 0, 3, 3, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
levelSprite:
.db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 3, 3, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0
.db 0, 3, 3, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0
.db 0, 3, 3, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 0, 0, 0, 0, 0, 3, 3, 3, 3, 0, 0, 0, 3, 3, 3, 3, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 3, 3, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 0, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 3, 3, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0
.db 0, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 3, 3, 3, 3, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 3, 3, 3, 3, 0, 0, 0, 3, 3, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
linesSprite:
.db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 3, 3, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 3, 3, 3, 3, 0, 0, 0, 3, 3, 3, 3, 3, 0, 0, 0, 3, 3, 0, 0, 0, 0
.db 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 3, 3, 3, 0, 3, 3, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0
.db 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 3, 3, 0, 0, 0, 0, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 3, 3, 0, 3, 3, 3, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 3, 3, 3, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 3, 3, 0, 0, 0, 0
.db 0, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 0, 0, 3, 3, 3, 3, 3, 3, 0, 0, 3, 3, 3, 3, 3, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

numbers:
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 2, 2, 2, 2, 0, 0
.db 0, 2, 2, 0, 0, 2, 2, 0
.db 0, 2, 2, 0, 0, 2, 2, 0
.db 0, 2, 2, 0, 0, 2, 2, 0
.db 0, 2, 2, 0, 0, 2, 2, 0
.db 0, 0, 2, 2, 2, 2, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 2, 2, 0, 0, 0
.db 0, 2, 2, 2, 2, 0, 0, 0
.db 0, 0, 0, 2, 2, 0, 0, 0
.db 0, 0, 0, 2, 2, 0, 0, 0
.db 0, 0, 0, 2, 2, 0, 0, 0
.db 0, 2, 2, 2, 2, 2, 2, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 2, 2, 2, 2, 0, 0
.db 0, 2, 2, 0, 0, 2, 2, 0
.db 0, 0, 0, 0, 2, 2, 0, 0
.db 0, 0, 0, 2, 2, 0, 0, 0
.db 0, 0, 2, 2, 0, 0, 0, 0
.db 0, 2, 2, 2, 2, 2, 2, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 2, 2, 2, 2, 0, 0
.db 0, 2, 2, 0, 0, 2, 2, 0
.db 0, 0, 0, 2, 2, 2, 0, 0
.db 0, 0, 0, 0, 0, 2, 2, 0
.db 0, 2, 2, 0, 0, 2, 2, 0
.db 0, 0, 2, 2, 2, 2, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 2, 2, 0, 0, 2, 2, 0
.db 0, 2, 2, 0, 0, 2, 2, 0
.db 0, 2, 2, 2, 2, 2, 2, 0
.db 0, 0, 0, 0, 0, 2, 2, 0
.db 0, 0, 0, 0, 0, 2, 2, 0
.db 0, 0, 0, 0, 0, 2, 2, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 2, 2, 2, 2, 2, 2, 0
.db 0, 2, 2, 0, 0, 0, 0, 0
.db 0, 2, 2, 2, 2, 2, 0, 0
.db 0, 0, 0, 0, 0, 2, 2, 0
.db 0, 0, 0, 0, 0, 2, 2, 0
.db 0, 2, 2, 2, 2, 2, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 2, 2, 2, 2, 0, 0
.db 0, 2, 2, 0, 0, 0, 0, 0
.db 0, 2, 2, 2, 2, 2, 0, 0
.db 0, 2, 2, 0, 0, 2, 2, 0
.db 0, 2, 2, 0, 0, 2, 2, 0
.db 0, 0, 2, 2, 2, 2, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 2, 2, 2, 2, 2, 2, 0
.db 0, 0, 0, 0, 0, 2, 2, 0
.db 0, 0, 0, 0, 0, 2, 2, 0
.db 0, 0, 0, 0, 2, 2, 0, 0
.db 0, 0, 0, 0, 2, 2, 0, 0
.db 0, 0, 0, 0, 2, 2, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 2, 2, 2, 2, 0, 0
.db 0, 2, 2, 0, 0, 2, 2, 0
.db 0, 0, 2, 2, 2, 2, 0, 0
.db 0, 2, 2, 2, 2, 2, 2, 0
.db 0, 2, 2, 0, 0, 2, 2, 0
.db 0, 0, 2, 2, 2, 2, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0
.db 0, 0, 2, 2, 2, 2, 0, 0
.db 0, 2, 2, 0, 0, 2, 2, 0
.db 0, 2, 2, 0, 0, 2, 2, 0
.db 0, 0, 2, 2, 2, 2, 2, 0
.db 0, 0, 0, 0, 0, 2, 2, 0
.db 0, 0, 2, 2, 2, 2, 0, 0
.db 0, 0, 0, 0, 0, 0, 0, 0

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