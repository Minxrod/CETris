 
;initialize 8bpp mode and palette
initLCD:
 call _RunIndicOff
 
;intialize palette from data
smcPaletteDataPtr=$+1
 ld hl, 0
 ld de, $e30200
 ld bc, 512 ;undefined colors likely never used
 ldir
  
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
 ld de,0
 ld (waitCounter),de
wr:
 ld de,(waitCounter)
 inc de
 ld (waitCounter),de
 bit bLcdIntLNBU, (hl)
 jr z, wr
 ret

waitCounter:
 .dl 0

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
 
;h=bpp id
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
;h = bpp id/setting
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
 bit 2, h
 jr z, noScale
 srl b ;half sizey
noScale:
 push de
 ld de,0
 ld d,l
 push de
 pop hl ;256L
 srl d
 rr e
 srl d
 rr e
 add hl,de ;add 64L: 320L total 
 pop de 
 add hl,de ;320Y+X
 ld de, (vramOffPtr) ;to second half of vRam, for double-buffering
 add hl,de ;320Y+x+vRam
;hl=vramptr ix=data b=sizeY c=sizeX a=pal 
;NEW: hl=data de=vramptr
 ex de,hl
 push ix
 pop hl
putSpriteYLoop:
 push bc
 ld b,c ;size x still
 push de ;save ptr to vram
putSpriteXLoop:
 push bc
smcPutPixel: ;add 1 for CALL address
 jp 0 ;ppbpp + something
putPixelReturn:
 pop bc
 djnz putSpriteXLoop
 ;hl=data de=vram
 ex de,hl ;de=data hl=vram
 pop hl
 ld bc,320
 add hl,bc ;move down 1 y unit
 ex de,hl 
 pop bc
 djnz putSpriteYLoop
 ret

ppbpp:
 jp putPixel1bpp
 jp putPixel2bpp
 jp putPixel4bpp
 jp putPixel8bpp
 jp put1bppHalfScale
 jp put2bppHalfScale
 jp put4bppHalfScale
 jp put8bppHalfScale
 jp put1bppNoTrans
 jp put2bppNoTrans
 jp put4bppNoTrans

;a=pal
;hl=data ptr
;de=vram ptr
;speed: 16cc/px 
putPixel8bpp:
 ld b,a ;b=palette				;1
 ld a,(hl) ;pixel color				;2
 or a,a ;check if no color/transparent color	;1
 jr z,transPixel8				;3
 add a,b ;add color to palette ofs		;1
 ld (de),a					;2
transPixel8:					
 ld a,b						;1
 inc de ;sprite data index			;1
 inc hl ;coords					;1
 jp putPixelReturn				;5

;a=pal
;hl=data ptr
;de=vram ptr
put8bppHalfScale:
 push bc
 ld b,a ;b=palette
 ld a,(hl) ;pixel color
 cp 0 ;check if no color/transparent color
 jp z,transPixel8h
 add a,b ;add color to palette ofs
 ld (de),a
transPixel8h:
 inc hl ;sprite data index
 inc hl
 inc de ;vram ptr
 ld a,b 
 pop bc ;b=row c=width
 
 dec b
 jp nz,putPixelReturn ;return if not last x loc
 ;c is still the width of X
 ld b,c
skipRow8h:
 inc hl ;add byte width to hl to skip next row
 djnz skipRow8h
 ;not very efficient but it's replacing actually drawing the entire row
 jp putPixelReturn
 
;a=pal
;hl=data ptr
;de=vram ptr
;speed: ??cc/px 
putPixel4bpp:
 ld c,a ;save palette ofs
 ld a,(hl) ;get data
 and $F0
 rrca
 rrca
 rrca
 rr a ;can set z flag
 jr z, noPut1stPixel
 add a,c ;add palette ofs to color
 ld (de),a ;load first nibble + palette ofs
noPut1stPixel:
 inc de ;next coord
 
 ld a,(hl) ;data again
 and $0F ;can set z flag
 jr z,noPut2ndPixel
 add a,c ;add palette and color
 ld (de),a
noPut2ndPixel:
 inc hl ;next data chunk
 inc de ;next vram index
 ld a,c
 jp putPixelReturn

;a=pal
;hl=data ptr
;de=vram ptr
;speed: ??cc/px 
put4bppHalfScale:
 push bc
 ld c,a ;save palette ofs
 ld a,(hl)
 srl a
 srl a
 srl a
 srl a
 jr z, noPutPixelh
 add a,c ;add palette ofs to color
 ld (de),a ;load nibble + palette ofs
noPutPixelh:
 inc de ;next vram index
 inc hl ;next data chunk
 ld a,c
 pop bc
 
 dec b
 jp nz, putPixelReturn ;return if not last x loc
 ;c is still the width of X
 ld b,c
skipRow4h:
 inc hl
 djnz skipRow4h
 jp putPixelReturn

;a=pal
;hl=data ptr
;de=vram ptr
;speed: ~21cc/px (86cc/4px)
putPixel2bpp:
 ld ixl,a					;2
 ld a,(hl)					;2
 ld b,4						;2
putPixel2bit:					;
 rlca						;1*4
 rlca						;1*4
 ld c,a						;1*4
 and $03					;2*4
 jr z, trans2bit				;3*4
 add a,ixl ;add pal and ofs			;2*4
 ld (de),a					;2*4
trans2bit:					;
 ld a,c ;restore unmasked value			;1*4
 inc de						;1*4
 djnz putPixel2bit				;4*4
 ld a,ixl					;2
 inc hl						;1
 jp putPixelReturn				;5

;a=pal
;hl=data ptr
;de=vram ptr
;speed: ??cc/px 
put2bppHalfScale:
 push bc
 ld c,(hl)
 push hl
 ld h,a ;save palette ofs
 ld b,2
putPixel2h:
 xor a ;a=0
 rlc c
 rla
 rlc c
 rla ;get 2 bits from c into a
 rlc c
 rlc c ;skip next 2 bits
 or a
 jr z, trans2h
 add a,h ;add pal and ofs
 ld (de),a
trans2h:
 inc de
 djnz putPixel2h
 ld a,h
 pop hl
 inc hl
 pop bc
 
 dec b
 jp nz, putPixelReturn ;return if b!=1
 ld b,c
skipRow2h:
 inc hl
 djnz skipRow2h
 jp putPixelReturn
 
;a=palette ofs
;hl=data ptr
;de=vram ptr
putPixel1bpp: 
 ld c,(hl)
 ld b,8
 ;new:
 ;c=bit data
 ;b=pixel count
putPixelBit:
 rlc c ;get bit from c
 ;if bit is zero, pixel is transparent.
 jr nc, noPutPixelBit ;skip to inc location
 ld (de),a ;save to location
noPutPixelBit:
 inc de ;next
 djnz putPixelBit
 inc hl ;done with this byte of data
 jp putPixelReturn
 
put1bppHalfScale:
 push bc
 ld c,(hl)
 ld b,4
 ;new:
 ;c=bit data
 ;b=pixel count
putPixelBith:
 rlc c ;get bit from c
 ;if bit is zero, pixel is transparent.
 jr nc, noPutPixelBith ;skip to inc location
 ld (de),a ;save to location
noPutPixelBith:
 inc de ;next
 rlc c
 djnz putPixelBith
 inc hl ;done with this byte of data
 pop bc
 
 dec b
 jp nz,putPixelReturn ;return if b!=1
 ld b,c
skipRow1h:
 inc hl
 djnz skipRow1h
 jp putPixelReturn
 
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

 ;drawOneBlockHalfScale: 
 ;ld ix, smcScaleBPP
 ;set 1,(ix+1)
 ;jr drawOneBlockShared
 
drawOneBlockNoGrid:
 ;ld ix, smcScaleBPP
 ;res 1,(ix+1)
 ;jr drawOneBlockShared

;drawOneBlockShared:
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
 ;smcScaleBPP:
 ;res 3, h ;default is res: smc to SET if half
 call drawSpriteCustomBPP
 ;pop hl ;restore old coordinates
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
;note: only sets 7st bit
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
 ret z ;also return if it should be ignored always
 sla e
 
 ld hl,drawJumps
 add hl,de ;jumps + 4e (since e is already *4 from 2x sla e)
 
 jp (hl)
 ret ;implied return from any of the jump methods
 
;jump table for various object types
;all should accept ix as a pointer to data
drawJumps:
 jp 0; drawField 
 jp drawText
 jp draw24BitNumber
 jp drawSpriteObject
 jp 0 ;drawCurrentHold
 jp 0 ;drawPreview
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
 jp 0 ;undefined
 jp drawCustom
 jp drawMap
 ret ;this one is just aesthetic
 
sharedDSO:
 jp drawSpriteObj

drawCustom:
 ld hl,(ix+iDataPTR)
 
 jp (hl) ;the rest is up to special drawing code
  
blockDataPTR:
 .dl 0 

drawBlock: 
smcDMB:
 ld ix,0
 ld a,0
skipBackgroundBixa:
 ld h,0
 ld c,0
 ld b,0
 call drawSpriteCustomBPP
 ret
smcDMB_ix = smcDMB + 2
smcDMB_a = smcDMB + 6
smcDMB_h = smcDMB + 8
smcDMB_c = smcDMB + 10
smcDMB_b = smcDMB + 12

drawTextColor:
 .db 0
drawTextBG:
 .db 0

;inputs: ix = data ptr
drawText:
 ld hl,(ix+iDataPTR)
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
smcFontDataPtr=$+2
 ld ix,0
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
 ld hl,(ix+iDataPTR)
 ld a,(hl)
 or a,a
 sbc hl,hl
 ld l,a ;hl = number
 ;jump to shared code
 jr drawNumShared

draw24BitNumber:
 ld hl,(ix+iDataPTR)
 
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
 ld hl,(ix+iDataPTR)
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
 call drawSpriteCustomBPP
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
 ld bc,(ix+iDataPTR)
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
 
 ld hl,(ix+iDataPTR)
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
 ld hl,(ix+iDataPTR)
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
 
 ld hl,(ix+iDataPTR)
 
 ld a,(menuSelection)
 ;hl: pointer to string list
 ;a: selection in list
getStringInList:
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
 ;or a,a
 ;ld de,SSS
 ;sbc hl,de ;hl=ofs from SSS
 ret
 
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

 ld hl,(ix+iDataPTR)
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

;input:
;ix = obj data ptr
drawMapWithBG:
 ld hl,(ix+iDataPTR)
 
;input:
;a = block size (in pixels) [0,127]
;b = bits/pixel [0,3]
;destroys:
;c = copy of a
;returns:
;a = block size (in bytes)
getBlockSizeBytes:
 ld c,a
 ld a,b
 xor a,$03
 ld b,a
 inc b
 ld a,c
 sla a
getBlockSizeBytesLoop:
 srl a
 djnz getBlockSizeBytesLoop
 ret
 
;input:
;ix = obj data ptr
drawMap:
 ld a,(ix+iDataA)
 ld (smcDrawMapB),a
 
 ld hl,(ix+iExtTile)
 ld b,(hl)
 call getBlockSizeBytes
 ld (smcDrawMapC),a
 ld c,a ; save for later 

 ld de,0
 ld d,(ix+iDataXH)
 ld e,(ix+iDataXL)
 ld (smcDM_x),de
 ld (smcDMy_x),de
 
 ld d,0
 ld e,(ix+iDataY)
 ld (smcDM_y),de
 
 ld a,(hl)
 ld (smcDrawMapH),a
 
 ld hl,(ix+iExtMap)
 ;at this point:
 ;hl = ptr to map data
 ;ix = obj data ptr
 ;smcDM b,c,h are set
 ;ix,a depend on tile
 ;stack: y,x (coords)
 ld b,(ix+iDataH)
drawMapLoopY:
 ld c,b
 ld b,(ix+iDataW)
smcDMy_x = $ + 1
 ld de,0
 ld (smcDM_x),de
drawMapLoopX:
 push bc

 ld de,0 
 ld e,(hl) ;tile id
 push hl
 ld hl,(ix+iExtTileset)
 add hl,de
 add hl,de ;points to sprite/palette pair
 ld b,(hl) ;block/sprite id
 inc hl
 ld a,(hl) ;palette
 ld (smcDrawMapTileA),a

 ld hl,(ix+iExtTile)
 inc hl ;points to sprite data
 ld a,(smcDrawMapC)
 ld e,a ; width of block in bytes
 ld d,(ix+iDataA)
 mlt de ;de = block size in bytes. assume size <256
 ld d,b ;sprite data id
 mlt de ;should be offset from drefSprite
 add hl,de ;points to sprite data
 ld (smcDrawMapTileIX),hl ;this is all set up now
 
smcDM_x = $ + 1
 ld de,0
smcDM_y = $ + 1
smcDrawMapH=$+2
 ld hl,0 ;21 LL HH UU
smcDrawMapB=$+1
 ld b,0
smcDrawMapC=$+1
 ld c,0
 push ix
 bit 0,(ix+iDataPTRL) ;check if background block is needed
 jr z,noBackgroundTile
 push hl
 push de
 push bc

 ld a,(ix+iDataPTRH) ; null block pallete, ig 
 ld ix,(ix+iExtTile)
 inc ix
 call drawSpriteCustomBPP
;call drawNullMinoBlock
 pop bc
 pop de
 pop hl 
noBackgroundTile:
smcDrawMapTileIX=$+2
 ld ix,0
smcDrawMapTileA=$+1
 ld a,0
 call drawSpriteCustomBPP
;call drawMinoBlock
 pop ix
 
 ld hl,(smcDM_x)
 ld de,0
 ld e,(ix+iDataA) ;tile size
 add hl,de
 ld (smcDM_x),hl
 
 pop hl
 inc hl
 pop bc
 djnz drawMapLoopX
 ld b,c
 
 push hl
 ld hl,(smcDM_y)
 ld de,0
 ld e,(ix+iDataA) ;tile size
 add hl,de
 ld (smcDM_y),hl
 pop hl

 dec b
 jp nz, drawMapLoopY 
;djnz drawMapLoopY
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
 call checkKeyA
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

checkKeyA:
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
 rlca ;result: carry if pressed
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

checkKey:
 call scanKeys
 ld a,-1
 ld (smcLastPress),a ;if no press, default -1
 ld b,64
rkeys:
 push bc
 ld a,b
 dec a
 call checkKeyA
 pop bc
 jr nc,noSetLastPress
 ld a,b
 dec a
 ld (smcLastPress),a
noSetLastPress:
 djnz rkeys
smcLastPress = $+1
 ld a,-1 ;a is key id pressed
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
 
;this is needed to give important menu jumps 
;fixed addresses to call from the data file, 
;which doesn't have access to the main program's 
;data. That way, menus can be designed separately
;from the program and allow for more customization.
;this will be copied to PSS+2048
menuJumps:
; jp mainMenu				;return to main menu
; jp initGame				;start game
 jp 0
 jp 0
 jp exit					;exit program
 jp activeMenu				;"runs" a menu object
 jp getStringInList			;gets [a] in menu/list
 jp getStringPTRSelection	;gets selected item
 jp setNumber				;select 8bit number
 jp drawObject				;draw given object
 jp checkKey				;keypress to [a]
 jp swapVRamPTR				;exactly what it says
menuJumpsEnd:
