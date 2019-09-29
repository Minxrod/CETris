#include "includes\ti84pce.inc"

; Credits:
; Minxrod - Primary developer
; Michael - troubleshooting

 .assume ADL=1
 .org userMem-2
 .db tExtTok,tAsm84CeCmp

PSS = plotSScreen
field = PSS
hold = PSS + 256
curX = PSS + 266
curY = PSS + 267
curR = PSS + 268
curT = PSS + 269
timerT = PSS + 270 ;to 272
lockTimer = PSS + 273
curPAL = PSS + 274
curBAG = PSS + 275
curBlock = PSS + 276

main:
 call initLCD
 call initGame
 
 call resetLCD
 ret
 
initGame:
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
 ;check exit
 ld a,(kbdG1)
 bit kbitDel, a
 ret nz
 call update
 call draw
 jp game
 ret

update:
 ld hl,(timerT)
 inc hl
 ld (timerT),hl
 bit 5,l
 jr nz, drop
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
 jr nz, drop
 ret
 
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
 
newBlock:
 ld a,5
 ld (curX),a
 ld a,0
 ld (curY),a
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

checkRotationLeft:
 ld hl, curBlock
 ld b,4
checkBlocksRotateLeft:
 push bc
 push hl
 ld a,(hl)
 ld e,a
 inc hl
 ld a,(hl)
 neg
 ld d,a
 push de
 pop hl
 call checkBlock
 cp 0
 pop hl
 pop bc
 jp nz,noRotationLeft
 inc hl
 inc hl
 djnz checkBlocksRotateLeft
;rotation is a go
 ld b,4
 ld hl, curBlock
rotateBlocksLeft:
 push bc
 push hl
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
 pop hl
 inc hl
 inc hl
 pop bc
 djnz rotateBlocksLeft
 ld a, -1
 ld (lockTimer),a
noRotationLeft:
 ret
 
checkRotationRight:
 ld hl, curBlock
 ld b,4
checkBlocksRotateRight:
 push bc
 push hl
 ld a,(hl)
 neg
 ld e,a
 inc hl
 ld a,(hl)
 ld d,a
 push de
 pop hl
 call checkBlock
 cp 0
 pop hl
 pop bc
 jp nz,noRotationRight
 inc hl
 inc hl
 djnz checkBlocksRotateRight
;rotation is a go
 ld b,4
 ld hl, curBlock
rotateBlocksRight:
 push bc
 push hl
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
 pop hl
 inc hl
 inc hl
 pop bc
 djnz rotateBlocksRight
 ld a, -1
 ld (lockTimer),a
noRotationRight:
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
random:
 ld a,r
 and $07
 ret 
 
tSpriteID:
 .db 0
tSpritePAL:
 .db 0
 
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

paletteData:
.dw $7fff, $6318, $3def, $0000
;dw $00ff, $0ff0, $ff00, $f00f
.dw $7fff, $4f7d, $029d, $1d39
.dw $7fff, $3a57, $1d39, $0000
.dw $7fff, $7f21, $7de4, $4402
.dw $7fff, $7fff, $7fc0, $7f21
.dw $7fff, $5b83, $12c9, $3def
.dw $7fff, $66fc, $5134, $0000
.dw $7fff, $7eb9, $7464, $4402
paletteDataEnd: