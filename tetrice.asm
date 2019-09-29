#include "includes\ti84pce.inc"

; Credits:
; Minxrod - Primary developer
; Michael - troubleshooting

 .assume ADL=1
 .org userMem-2
 .db tExtTok,tAsm84CeCmp

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
main:
 call initLCD
 call initGame
 
 call resetLCD
 ret
 
initGame:
 ld a,1
 ld (level),a
 ld a,7
 ld (holdT),a
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
 call checkLines
 ret

hardDrop:
 ld a,(lockTimer)
 cp -1
 jr nz, hdrop
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
 ld (lockTimer),a
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
 ld e,a
 ld hl,(lines)
 add hl,de
 ld (lines),hl
 ld a,10
 call _DivHLbyA
 ld (level),hl
 
 ld a,(linesClear)
 ld de,0
 ld e,a
 ld hl,pointsPerLine
 add hl,de
 ld a,(hl) ;score to add for lines cleared
 ld de,0
 ld e,a
 ld hl,0
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
 ret
 
clearLine:
 ;hl=end of cleared line
 push hl
 ld bc,field
 ld de,10
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
 ld bc,10
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
 call random
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
 halt
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
 ld a, -1
 ld (lockTimer),a
blockTooFarLeft:
 ret

squareKickLeft:
 ld hl, kicksO
 ret ;still not worth it 
lineKickLeft:
 ld hl, kicksI
 jr sharedKickLeft
 
checkRotationLeft:
 ld a,(curT)
 cp 3
 jr z, squareKickLeft
 cp 0
 jr z, lineKickLeft
 
 ld hl, kicksNormal
sharedKickLeft:
 push hl ;save table address
 
 ld a,(curR)
 cp a,0
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
 ;get y
 ld a,(hl)
 ld e,a
 ld a,(rY)
 add a,e
 cp 20
 jr nc, noRotationLeft
 ld e,a
 inc hl
 ;get x
 ld a,(hl)
 neg
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
 djnz checkBlocksRotateLeft
;rotation is a go
 ld b,4
 ld hl, curBlock
rotateBlocksLeft:
 push bc
 ld a,(hl)
 ld e,a
 inc hl
 ld a,(hl)
 neg
 ld d,a
 dec hl ;back to X location
 ld (hl),d
 inc hl
 ld (hl),e ;blocks have rotated: now do the next one
 inc hl
 pop bc
 djnz rotateBlocksLeft
 ld a, -1
 ld (lockTimer),a
 
 ld a,(curR)
 dec a
 and $03
 cp 0
 jr nz, skipOff4
 ld a,4
skipOff4:
 ld (curR),a
 pop hl
 
 ld a,(rX)
 ld (curX),a
 ld a,(ry) ;save the location of the successful turn.
 ld (curY),a
 ret
noRotationLeft:
 pop hl ;get offset
 
 ld de,-10
 call calculateRXY ;rxy stored - hl is already at next ofs
 push hl ;at next offset already
 
 ld hl, curBlock
 ld b,4
 
 ld a,(curT)
 cp 3
 jp z, doneLeft
 
 ld a,(rAttempt)
 inc a
 ld (rAttempt),a
 cp 5
 jp nz, checkBlocksRotateLeft
doneLeft:
 pop hl ;unneeded.
 ret
 
rotationTempHL:
 .db 0,0,0 ;3 bytes for HL
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
 add a, (hl) ;add x offset
 add hl,de
 sub (hl) ;subtract next offset
 or a
 sbc hl,de ;undo the addition to next offset (back to start)
 ld (rX), a ;save to special location
 inc hl ;now on Y value
 ld a, (curY)
 add a, (hl) ;add y offset
 add hl,de ;get next offset
 sub (hl)
 or a
 sbc hl,de ;undo again
 ld (rY), a
 inc hl ;hl is 2 past previous offset ->
 ret
 
squareKickRight:
 ld hl, kicksO
 ret ;too not worth it
lineKickRight:
 ld hl, kicksI
 jr sharedKickRight
 
checkRotationRight:
 ld a,(curT)
 cp 3
 jr z, squareKickRight
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
 ;get y
 ld a,(hl)
 neg
 ld e,a
 ld a,(rY)
 add a,e
 cp 20
 jr nc, noRotationRight ;out of bounds
 ld e,a
 ;get x
 inc hl
 ld a,(hl)
 ld d,a
 ld a,(rX)
 add a,d
 cp 10
 jr nc, noRotationRight ;also out of bounds
 ld d,a
 ld (rotationTempHL),hl
 push de
 pop hl
 call checkBlock
 cp 0
 jp nz,noRotationRight
 ld hl,(rotationTempHL)
 inc hl
 djnz checkBlocksRotateRight
;rotation is a go
 ld b,4
 ld hl, curBlock

rotateBlocksRight:
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
 djnz rotateBlocksRight
 
 ld a, -1
 ld (lockTimer),a
 pop hl ;unneeded
 
 ld a,(curR)
 inc a
 and $03
 ld (curR),a
 
 ld a,(rX)
 ld (curX),a
 ld a,(ry) ;save the location of the successful turn.
 ld (curY),a
 ret
noRotationRight:
 pop hl ;get offset
 
 ld de,10
 call calculateRXY ;new rxy stored - hl is already at next ofs
 push hl ;at next offset already
 
 ld hl, curBlock
 ld b,4
 
 ld a,(curT)
 cp 3
 jp z, doneRight
 
 ld a,(rAttempt)
 inc a
 ld (rAttempt),a
 cp 5
 jp nz, checkBlocksRotateRight
doneRight:
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
 ld a, -1
 ld (lockTimer),a
blockTooFarRight:
 ret

checkBlockDown:
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
 ld a,(curY)
 inc a
 ld (curY),a
 ret
blockTooFarDown:
 ld a,(lockTimer)
 cp -1
 jr nz,noLockRefresh
 ld a,10
 ld (lockTimer),a
noLockRefresh:
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
 ld de, vRam
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
 inc hl ;no level zero, please
 ld b, 2
 ld d, 160
 ld e, 32
 call draw24BitNumber
 
 ld hl,(lines)
 ld b, 4
 ld d, 160
 ld e, 56
 call draw24BitNumber
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
 .db 00
random:
 ld a,r
 and $07
 cp $07
 ret nz
 ld a,(randsav)
 inc a
 ld (randsav),a
 cp $07
 ret nz
 ld a,0
 ld (randsav),a
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
 .db  0, 0, -1, 0,  2, 0, -1, 0,  2, 0
 
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