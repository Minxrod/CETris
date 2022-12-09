#include "includes/ti84pce.inc"
#include "includes/tetrice.inc"

 .assume ADL=1
 .org userMem-2
 .db tExtTok,tAsm84CeCmp

;main program
main:
 ld (preserveSP),sp
 
 ld de,fontData
 ld (smcFontDataPtr),de
 ld de,paletteData
 ld (smcPaletteDataPtr),de 
 
 call initLCD ;screen init (requires data for palette) 

 ld hl,12345678
 call badDivHLBy10
 ld (testNum),hl
 
 ld ix,testMenu
 call drawObject
 call swapVRamPTR
test_select:
 call _GetCSC
 ld ix,0
 cp $21
 jr z, test_loop
 ld ix,testText 
 cp $22
 jr z, test_loop 
 ld ix,testNumber
 cp $1a
 jr z, test_loop
 ld ix,testSprite1
 cp $13
 jr z, test_loop
 ld ix,testSprite2
 cp $1b
 jr z, test_loop
 ld ix,testNumber8
 cp $1c
 jr z, test_loop
 ld ix,testBox
 cp $24
 jr z, test_loop
 ld ix,testSprite4
 cp $23
 jr z, test_loop 
 ld ix, testMap
 cp $14
 jr z, test_loop
 
 jr test_select
 
test_loop:
 call initLCD
test_loop_actual:
 inc ix
 dec ix
 
 jr z, noTestObject
 call drawObjectNoReset
noTestObject: 
 push ix ;keep pointer to test object
 ld ix, testSpd
 call drawObjectNoReset
 call swapVRamPTR
 
 call _GetCSC
 pop ix
 
 cp skUp
 jr z,incTestQuantity
 cp skDown
 jr z,decTestQuantity
 cp skRight
 jr z,incTestQuantity2
 cp skLeft
 jr z,decTestQuantity2
 cp skAdd
 jr z,incTestQuantityPtr
 cp skSub
 jr z,decTestQuantityPtr
 
 cp skDel
 jr nz,test_loop_actual
 
exit:
 ;restore state for ti-os
 call resetLCD
;call restoreKeyboard
 
 ld sp,(preserveSP)
 ret

incTestQuantity2:
 inc (ix+iDataH)
 jr waitForNoKey

decTestQuantity2:
 dec (ix+iDataH)
 jr waitForNoKey

decTestQuantity:
 dec (ix+iDataW)
 jr waitForNoKey

incTestQuantity:
 inc (ix+iDataW)
 jr waitForNoKey

incTestQuantityPtr:
 inc (ix+iDataPTRL)
 jr waitForNoKey

decTestQuantityPtr:
 dec (ix+iDataPTRL)
 jr waitForNoKey

waitForNoKey:
 call _GetCSC
 or a,a
 jr nz,waitForNoKey
 jr test_loop_actual

badDiv102:
 ;in: hl
 ;out: hl//10, a=hl%10
 push hl
 push hl
 pop de
 ;get low bit or something
 adc hl,hl
 adc hl,de
 adc hl,hl
 adc hl,hl ;12hl
 push hl
 pop de
 adc hl,hl
 adc hl,hl
 adc hl,hl
 adc hl,hl ;192
 adc hl,de ;204hl
 adc hl,hl
 adc hl,hl
 adc hl,hl
 adc hl,hl ;3264
 adc hl,de ;3276
 adc hl,hl ;6552
 adc hl,hl
 adc hl,hl
 adc hl,hl ;3264*16
 adc hl,de ;
 adc hl,hl ;
 adc hl,hl
 adc hl,hl
 adc hl,hl ;
 adc hl,de ;
 adc hl,hl ;
 pop de
 adc hl,de ;
 ;de = hl
 ;hl = 6553hl/65536
 ex de,hl
 or a,a
 sbc hl,de
 sbc hl,de
 sbc hl,de
 sbc hl,de
 sbc hl,de ;5
 sbc hl,de 
 sbc hl,de
 sbc hl,de
 sbc hl,de
 sbc hl,de ;10
 ld a,l
 cp 10
 ret c
 sub 10
 ;hl = A-10B
 ;de = A // 10
 ;a =  A  % 10
 ret


testString:
 .db "0123456789 Test str Hello world!",0

testNum:
 .dl $123456

preserveSP:
 .dl 0

testCompound:
 .db typeCompound
 .dw 0
 .db 0
 .db 0
 .dl testAll
 .db 9, 0

testAll:
testSpd:
 .db typeNumber
 .dw 0
 .db 0
 .db 1
 .dl waitCounter
 .db 8, 4

testText:
 .db typeString
 .dw 0 ; x
 .db 8 ; y
 .db 1 ; fg col
 .dl testString+20 ;ptr to text
 .db 0, 0 ; unused, bg col

testNumber:
 .db typeNumber
 .dw 0 ; x
 .db 8 ; y
 .db 1 ; fg col
 .dl testNum ;ptr to number
 .db 8, 4 ; unused, bg col

testSprite2:
 .db typeSprite2bpp
 .dw 0 ; x
 .db 8 ; y
 .db 12 ; palette offset
 .dl testSprite2bpp ;ptr to sprite data
 .db 3, 12 ; width (bytes), height (pixels)

testSprite1:
 .db typeSprite1bpp
 .dw 0 ; x
 .db 8 ; y
 .db 1 ; palette offset
 .dl fontData + 1 + 264 ;ptr to sprite data
 .db 1, 8 ; width (bytes), height (pixels)

testNumber8:
 .db typeNumber8
 .dw 0 ; x
 .db 8 ; y
 .db 1 ; fg col
 .dl testNum ;ptr to number
 .db 4, 4 ; unused, bg col

testSprite4:
 .db typeSprite4bpp
 .dw 0 ; x
 .db 8 ; y
 .db 8 ; palette offset
 .dl testSprite4bpp ;ptr to sprite data
 .db 8, 16 ; width (bytes), height (pixels)

testBox:
 .db typeBox
 .dw 0 ;x
 .db 8 ;y
 .db 5 ;color
 .dl 72 ;width
 .db 6, 40 ;bordercolor, height

testMenu:
 .db typeMenu
 .dw 0 ; x
 .db 8 ; y
 .db 1 ; Text color
 .dl testMenuText ;ptr to menu items
 .db 10, 0 ;number of items, index of cursor object

testMap:
 .db typeMap
 .dw 0 ; x
 .db 0 ; y
 .db 8 ; size of tile
 .db 0, 0, 0 ; flags (1 = use background block, 0 = don't), unused x2
 .db 26, 30 ;width (tiles), height (tiles)
testMapExtended:
 .db typeExtended ;do not draw
 .dl smbTile
 .dl smbTileset
 .dl smbMap

testMenuText:
 .db "0 - Baseline",0
 .db "1 - Text",0
 .db "2 - Number24",0
 .db "3 !- Sprite8",0
 .db "4 - Sprite4",0
 .db "5 - Sprite2",0
 .db "6 - Sprite1",0
 .db "7 - Box",0
 .db "8 - Number8",0
 .db "9 - Map",0

paletteData:
;.dw $00ff, $0ff0, $ff00, $f00f
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
.db $5f, $2e, $f6, $7e, $21, $65, $00, $00
.db $5f, $2e, $ff, $7f, $ff, $1e, $00, $00
.db $5f, $2e, $67, $7e, $21, $65, $00, $00
.db $5f, $2e, $42, $43, $a0, $02, $00, $00
.db $5f, $2e, $c4, $58, $64, $76, $a0, $35
paletteDataEnd:

testSprite2bpp:
;12x12
.db $55, $55, $55, $6a, $6a, $54, $65, $59
.db $54, $65, $59, $50, $6a, $59, $50, $55
.db $55, $40, $55, $55, $00, $55, $54, $00
.db $55, $50, $00, $55, $40, $3c, $50, $00
.db $3c, $00, $00, $00

testSprite4bpp:
;16x16
.db $00, $00, $11, $11, $22, $22, $33, $33
.db $00, $00, $11, $11, $22, $22, $33, $33
.db $00, $00, $11, $11, $22, $22, $33, $33
.db $00, $00, $11, $11, $22, $22, $33, $33
.db $44, $44, $55, $55, $66, $66, $77, $77
.db $44, $44, $55, $55, $66, $66, $77, $77
.db $44, $44, $55, $55, $66, $66, $77, $77
.db $44, $44, $55, $55, $66, $66, $77, $77
.db $88, $88, $99, $99, $aa, $aa, $bb, $bb
.db $88, $88, $99, $99, $aa, $aa, $bb, $bb
.db $88, $88, $99, $99, $aa, $aa, $bb, $bb
.db $88, $88, $99, $99, $aa, $aa, $bb, $bb
.db $cc, $cc, $dd, $dd, $ee, $ee, $ff, $ff
.db $cc, $cc, $dd, $dd, $ee, $ee, $ff, $ff
.db $cc, $cc, $dd, $dd, $ee, $ee, $ff, $ff
.db $cc, $cc, $dd, $dd, $ee, $ee, $ff, $ff

smbTile:
.db sp2bpp
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $0f, $00, $35, $03, $d5, $0d, $55
.db $0d, $55, $0d, $55, $35, $69, $d5, $95
.db $f0, $00, $5c, $00, $57, $00, $57, $30
.db $55, $dc, $65, $57, $59, $57, $55, $57
.db $00, $3f, $00, $d5, $03, $55, $01, $55
.db $3d, $55, $d5, $55, $d5, $55, $35, $55
.db $c3, $00, $cd, $c0, $75, $c0, $55, $cc
.db $55, $77, $55, $57, $55, $57, $55, $5c
.db $55, $55, $aa, $ab, $aa, $ab, $ff, $ff
.db $ab, $aa, $ab, $aa, $ab, $aa, $ff, $ff
.db $2a, $aa, $95, $55, $9d, $55, $95, $6a
.db $95, $af, $95, $ad, $95, $ad, $95, $7d
.db $aa, $a8, $55, $57, $55, $77, $a5, $57
.db $e9, $57, $6b, $57, $6b, $57, $ab, $57
.db $ff, $ff, $d5, $55, $ea, $a5, $d5, $a5
.db $d5, $a5, $d5, $a5, $d5, $a5, $d5, $a5
.db $ff, $ff, $55, $55, $55, $aa, $55, $96
.db $55, $96, $55, $96, $55, $96, $55, $96
.db $0d, $5a, $0d, $5a, $0d, $5a, $0d, $5a
.db $0d, $5a, $0d, $5a, $0d, $5a, $0d, $5a
.db $aa, $aa, $aa, $aa, $aa, $aa, $aa, $aa
.db $aa, $aa, $aa, $aa, $aa, $aa, $aa, $aa
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $3f, $0f, $ea, $fa, $aa
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $fc, $00, $ab, $f0, $aa, $af
.db $00, $15, $00, $55, $00, $fe, $03, $ba
.db $03, $be, $03, $ea, $00, $2a, $00, $f7
.db $50, $00, $55, $40, $b8, $00, $ba, $80
.db $ae, $a0, $bf, $c0, $aa, $00, $f0, $00
.db $55, $55, $55, $55, $55, $55, $55, $55
.db $55, $55, $55, $55, $55, $55, $55, $55
.db $0d, $65, $03, $59, $00, $d6, $00, $d5
.db $00, $3f, $00, $00, $00, $00, $00, $00
.db $55, $55, $65, $55, $a9, $5a, $5a, $aa
.db $55, $a5, $d5, $57, $3d, $5c, $03, $f0
.db $65, $55, $95, $55, $a5, $59, $6a, $a5
.db $5a, $95, $55, $55, $f5, $5f, $0f, $f0
.db $55, $70, $55, $5c, $55, $57, $55, $54
.db $55, $7c, $d7, $c0, $3c, $00, $00, $00
.db $aa, $ab, $aa, $ab, $aa, $ab, $ff, $ff
.db $ab, $aa, $ab, $aa, $ab, $aa, $ff, $ff
.db $95, $56, $95, $56, $95, $55, $95, $56
.db $95, $56, $9d, $55, $95, $55, $ff, $ff
.db $bf, $57, $b5, $57, $f5, $57, $95, $57
.db $b5, $57, $f5, $77, $55, $57, $ff, $ff
.db $d5, $a5, $d5, $a5, $d5, $a5, $d5, $a5
.db $d5, $a5, $d5, $a5, $ff, $ff, $0f, $ff
.db $55, $96, $55, $96, $55, $96, $55, $96
.db $55, $96, $55, $96, $ff, $ff, $ff, $ff
.db $55, $65, $55, $65, $55, $65, $55, $65
.db $55, $65, $55, $65, $55, $65, $55, $65
.db $00, $03, $00, $0e, $00, $3a, $00, $ea
.db $03, $aa, $0e, $aa, $3a, $aa, $ea, $aa
.db $aa, $ba, $aa, $fe, $aa, $fe, $aa, $fe
.db $be, $fe, $be, $ba, $be, $aa, $be, $aa
.db $c0, $00, $b0, $00, $ac, $00, $ab, $00
.db $aa, $c0, $aa, $b0, $aa, $ac, $aa, $ab
.db $03, $f7, $0f, $f5, $0a, $d9, $0a, $95
.db $0a, $55, $00, $54, $03, $f0, $0f, $f0
.db $df, $c0, $5f, $f0, $67, $a0, $56, $a0
.db $55, $a0, $15, $00, $0f, $c0, $0f, $f0
.db $95, $55, $6a, $aa, $6a, $aa, $6a, $aa
.db $6a, $aa, $6a, $aa, $6a, $aa, $6a, $aa
.db $79, $56, $b6, $ab, $b6, $ab, $b6, $ab
.db $b7, $ab, $bb, $fe, $b5, $57, $b6, $ab
.db $6a, $aa, $6a, $aa, $fa, $aa, $5f, $aa
.db $65, $ff, $6a, $57, $6a, $ab, $bf, $fe
.db $b6, $ab, $b6, $ab, $da, $ab, $da, $ab
.db $6a, $ab, $6a, $ab, $6a, $af, $7f, $fe

smbTileset:
.db $00, $50
.db $01, $44
.db $02, $44
.db $03, $44
.db $04, $44
.db $05, $40
.db $06, $48
.db $07, $48
.db $08, $4c
.db $09, $4c
.db $0a, $4c
.db $0b, $4c
.db $0c, $4c
.db $0d, $4c
.db $0e, $50
.db $0f, $50
.db $10, $44
.db $11, $44
.db $12, $44
.db $13, $44
.db $14, $44
.db $15, $48
.db $16, $48
.db $17, $48
.db $18, $4c
.db $19, $4c
.db $1a, $4c
.db $1b, $4c
.db $1c, $4c
.db $1d, $4c
.db $1e, $50
.db $1f, $50
.db $03, $4c
.db $01, $4c
.db $02, $4c
.db $04, $4c
.db $10, $4c
.db $20, $40
.db $21, $40
.db $22, $40
.db $23, $40

smbMap:
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $01, $02, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $03
.db $10, $10, $04, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $11
.db $12, $13, $14, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $01, $02
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $03
.db $10, $10
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $11
.db $12, $13
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $06, $07, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $16, $17, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00
.db $06, $07, $00, $00, $00, $00, $00, $00
.db $05, $05, $06, $07, $05, $05, $06, $07
.db $05, $05, $00, $00, $00, $00, $00, $00
.db $00, $00
.db $16, $17, $00, $00, $00, $00, $00, $00
.db $15, $15, $16, $17, $15, $15, $16, $17
.db $15, $15, $00, $00, $00, $00, $00, $00
.db $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $08, $09
.db $00, $00, $0c, $0d, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $00, $00, $00, $00, $00, $00, $00, $00
.db $18, $19
.db $00, $1b, $0b, $1c, $1d, $00, $00, $00
.db $0e, $0f, $00, $00, $00, $00, $00, $00
.db $21, $22, $00, $00, $00, $00, $00, $00
.db $0a, $1a
.db $1b, $0b, $0b, $0b, $0b, $1d, $00, $00
.db $1e, $1f, $00, $00, $00, $00, $00, $20
.db $24, $24, $23, $00, $00, $00, $00, $00
.db $0a, $1a
.db $25, $26, $25, $26, $25, $26, $25, $26
.db $25, $26, $25, $26, $25, $26, $25, $26
.db $25, $26, $25, $26, $25, $26, $25, $26
.db $25, $26
.db $27, $28, $27, $28, $27, $28, $27, $28
.db $27, $28, $27, $28, $27, $28, $27, $28
.db $27, $28, $27, $28, $27, $28, $27, $28
.db $27, $28
.db $25, $26, $25, $26, $25, $26, $25, $26
.db $25, $26, $25, $26, $25, $26, $25, $26
.db $25, $26, $25, $26, $25, $26, $25, $26
.db $25, $26
.db $27, $28, $27, $28, $27, $28, $27, $28
.db $27, $28, $27, $28, $27, $28, $27, $28
.db $27, $28, $27, $28, $27, $28, $27, $28
.db $27, $28

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


