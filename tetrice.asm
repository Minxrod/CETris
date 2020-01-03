#include "includes\ti84pce.inc"
#include "includes\tetrice.inc"

 .assume ADL=1
 .org userMem-2
 .db tExtTok,tAsm84CeCmp

;main program
;this is where the program starts + lotsa setup stuff
main:
 ld (preserveSP),sp
 ld hl, PSS
 ld de, PSS + 1
 ld bc, 512
 ld (hl),0
 ldir
 
 ;save loading
 call loadSaveVar ;get ptr to save data from appvar
 inc de
 inc de ;past size of appvar
 ld (saveDataPTR),de
 call loadSave ;actually read save data
 
 ;initialize various data
 call initData	;important info + appvar load
 call initLCD ;screen init (requires data for palette)
 
 call defaultInfo ;default game rules/information
 call initDataCustom ;appvar custom setup
 
 jp mainMenu
 
exit:
 ;save appvar to archive
 call saveSave
 
 ;restore state for ti-os
 call resetLCD
 call restoreKeyboard
 
 ld sp,(preserveSP)
 ret

error:
 call saveSave

 call resetLCD
 call restoreKeyboard
 
 ld sp,(preserveSP) 
 
 ld hl,errorString
 ld de,appErr1
 ld bc,errorStringEnd - errorString
 ldir

 call _ErrCustom1
 ret
 
errorString:
 .db "NO CETrisDT",0
errorStringEnd:
 
initDataCustom:
 jp initDataStart
 
preserveSP:
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
 ld ix,(drefMenu)
 jp activeMenu
 
initGame:
 ld ix,rules
 set rbitGameCont, (ix+rfBasic) ;game is going
 res rbitGameWon, (ix+rfBasic) ;game not won
 
 ld a,0
 ld (lines),a
 ld (linesToNextLevel),a
 
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
 ld a,fieldHeight
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
 
incGarbage:
 ld ix,rules
 bit rbitGarbageRising,(ix+rfExtra)
 ret z ;garbage isn't rising so don't do stuff
 
 ;increment garbage timer
 ld hl,rules+rRGT
 ld a,(garbageTimer)
 inc a
 ld (garbageTimer),a
 cp (hl) ;check if timer needs to inc queue garbage
 ret c
 xor a
 ld (garbageTimer),a
 
 ld a,(queuedGarbage)
 inc a
 ld (queuedGarbage),a
 ret

;input: ix=rules
raiseGarbage:
 ld a,(queuedGarbage)
 cp 0
 ret z ;don't raise garbage if there's none to raise!
 ld b,a
 xor a
 ld (queuedGarbage),a
 
;raise field and generate garbage as many times as needed
raiseOneRow:
 push bc
 ld de,field
 ld hl,field+10
 ld bc,245
 ldir ;copy fielddata up/shift field up
 
 ;zero last row
 ld hl,fieldHeight - 1 * 10 + field
 ld de,fieldHeight - 1 * 10 + field + 1
 ld bc,9
 ld (hl),NULL_BLOCK
 ldir
 
 ld b, (ix+rRGD) ;rising garbage density
 ld hl,fieldHeight - 1 * 10 + field ;last row of field
 call generateGarbageRow
 pop bc
 djnz raiseOneRow
 
 ld hl,(refField)
 res redrawObjBit, (hl) ;ix+0 ix+objtype
 ld hl,curStatus
 set csGarbageBit, (hl) ;garbage rose on this frame = field redraw this frame
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
 set rbitGameWon, (hl) ;win by lines clear
 
skipEndCheck:
 
 xor a
 ld (PSS+444),a
checkGravity:
 ld a,(level)
 dec a
 cp 20
 jr c,noLevelOverflow
 ld a,19 ;max level speed [0-19]
noLevelOverflow:
 ld c,a
 add a,a
 add a,c ;3a: ofs from speedcurve
 ld de,0
 ld e,a 
 ld hl,speedCurve
 add hl,de
 ld hl,(hl) ;hl is the speed in 8.16 fp rows/frame
 ex de,hl ;de is speed
 ld hl,(timerT)
 add hl,de
 ld (timerT),hl
checkGravityLoop:
 ld hl,(timerT)
 ld de,65536 ;threshold for one row drop
 or a,a
 sbc hl,de
 jr c, noDropGravity
 ;if carry, hl was less than 65536, so timer's not done
 ld a,(PSS+444)
 inc a
 ld (PSS+444),a
 ld (timerT),hl
 call checkBlockDown
 jr checkGravityLoop ;must check for more than 1 row/frame
noDropGravity:
;drops are done/unneeded

 ld ix,buttonSoft
 call checkKeyDown
 jr c, userdrop
 ld ix,buttonHard
 call checkKeyDown
 jr c, harddrop
dropReturn:

 call incGarbage
 
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
 
 ret
gameEnd:
 ld ix,rules
 res rBitGameCont, (ix+0) ;game is not going
 ret
 
checkLineClears:
 ld b, fieldHeight
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
 ret
 
checkLines:
 ;check if garbage rises
 ld ix,rules
 bit rbitGarbageRising, (ix+rfExtra)
 call nz,raiseGarbage

 ld hl,pointsPerLine
 ld (smcPointsPerLine),hl ;no spin=regular scoring
 ld hl,curStatus ;last move was rotate w/T
 scf ;set carry, in case no call checktspin
 bit csRotateTBit,(hl)
 call nz,checkTSpin

 ld a,0
 ld (linesClear),a
 call checkLineClears

 call updateB2B
 
 ld a,(linesClear)
 or a,a
 jr z, noLinesSpin ;is line clear update needed?
 
 ;if yes, must redraw field, update scores, etc.
 ld ix, (refField)
 res redrawObjBit, (ix+iDataType)
 
 ld ix, (refLevel)
 res redrawObjBit, (ix+iDataType)
 
 ld ix, (refLines)
 res redrawObjBit, (ix+iDataType)
 
 ld hl, curStatus
 set csClearLineBit, (hl) ;clear bit 

noLinesSpin: ;needs to update score for spins etc.
 ld ix, (refScore)
 res redrawObjBit, (ix+iDataType)
 
 ld ix,rules
 bit rbitCascadeGravity,(ix+rfExtra)
 call nz,cascadeBlocks
 
 ld a,(linesClear)
 ld de,0
 ld e,a ;de is lines cleared
 ld hl,(lines)
 add hl,de
 ld (lines),hl
 
 ld a,(linesToNextLevel)
 add a,e ;e=lines cleared
 ld (linesToNextLevel),a
 
smcPointsPerLine = $+1 
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
 push hl
 pop de ;50*score=de
 add hl,hl ;100*score
 
 ld a,(lineClearInfo)
 bit lcBackToBack,a
 jr z,noB2BBonus
 bit lcPreviousB2B,a ;only works if "back to back"
 jr z,noB2BBonus
 add hl,de ;150*score (back2back bonus)
noB2BBonus:
 
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
 
updateB2B:
 ld hl,lineClearInfo
 ld a,(linesClear)

 ;save previous state
 res lcPreviousB2B,(hl)
 bit lcBackToBack,(hl)
 jr z,noPrevB2B
 set lcPreviousB2B,(hl)
noPrevB2B:

 ;check for potential back-to-back status on this line clear
 bit lcTSpin,(hl)
 jr nz,checkTetrisB2B ;no t spin = check lines amt
 ld a,(linesClear)
 or a,a
 jr z,B2BDone ;unaffected by no line-t spin
 set lcBackToBack,(hl)
 jr B2BDone ;set from T spin and line clear

checkTetrisB2B:
 res lcBackToBack,(hl) ;default unset
 ld a,(linesClear)
 cp 4
 jr c,B2BDone ;jump if not tetris
 set lcBackToBack,(hl) ;set if tetris or better
B2BDone:
 ;b2b set if tetris or tspin, reset otherwise
 ret
 
 
tSpinCheckLocation:
 .db -1,-1
 .db  1,-1
 .db  1, 1
 .db -1, 1
 .db -1,-1
 
tSpinCLCount:
 .db 0
 
;sources:
;https://tetris.fandom.com/wiki/T-Spin
;https://tetris.wiki/Tetris_Guideline
;https://harddrop.com/wiki/T-Spin
;https://harddrop.com/wiki/Talk:Tetris_Guideline
;there are many variations to t-spin checks.
;CETris is attempting to conform to the guideline
;check.

checkTSpin:
 ld hl,lineClearInfo
 res lcTSpin,(hl)

 xor a
 ld (tSpinCLCount),a
 ld b,4
 ld de,tSpinCheckLocation
checkTSpinLoop:
 push bc
 call hlFromBlockAndPTR
 
 push de
 call checkBlock
 cp NULL_BLOCK
 jr z,emptyCheck
 ld hl,tSpinCLCount ;found block
 inc (hl)
emptyCheck:
 pop de
 pop bc
 djnz checkTSpinLoop
 
 ld a,(tSpinCLCount)
 cp 3
 ret c 
 ;less than 3 corner blocks: invalid T spin
 ;note: set carry -> no t-spin
 ld hl,lineClearInfo
 set lcTSpin,(hl)
 
 ;using the 5th rotation state (triple spin kick)
 ;is always a regular T spin, never a mini
 ld hl,curStatus
 bit csWallKicked,(hl)
 jr nz, regularTSpin
 
 ;if one cell next to pointing side filled: mini
 ld a,(curR)
 add a,a
 ld hl,tSpinCheckLocation
 ld de,0
 ld e,a
 add hl,de ;points to pointing side's info
 ex de,hl ;de points to info
 call hlFromBlockAndPTR
 push de
 call checkBlock
 pop de
 cp NULL_BLOCK
 jr z,isTMini ;implies one block unoccupied = mini
 call hlFromBlockAndPTR
 push de
 call checkBlock
 pop de
 cp NULL_BLOCK
 jr z,isTMini ;implies one block unoccupied = mini
 
 ;if TST kick: standard
regularTSpin:
 ld hl,pointsPerTLine
 ld (smcPointsPerLine),hl
 or a,a ;clear carry
 ret

;t-spin mini lol
isTMini:
 ld hl,pointsPerTMini
 ld (smcPointsPerLine),hl
 or a,a ;clear carry
 ret
 
;input:
;de=ptr to ofs
;output:
;h=x+ofs
;l=y+ofs
hlFromBlockAndPTR:
 ex de,hl ;hl is ptr, de is output
 ld a,(curX) ;x location
 add a,(hl) ;add check ofs
 ld d,a
 inc hl
 
 ld a,(curY) ;x location
 add a,(hl) ;add check ofs
 ld e,a
 inc hl
 ex de,hl
 ret
 
clearLine:
 ld a,b
 cp 1 ;last row is being check and is clear
 jr nz, skipGameEndR0 ;not last row
 ld ix,rules
 bit rbitRow0Clear, (ix+rfWin)
 jr z, skipGameEndR0 ;if row 0 is not the win condition, don't end game
 res rbitGameCont, (ix+rfBasic)
 set rbitGameWon, (ix+rfBasic) ;you've won!
 
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
 
cascadeBlocks:
 ld hl,(refField)
 res redrawObjBit, (hl) ;only applies to
 
 ld b,fieldHeight-1
applyGravity:
 push bc
 
 ;apply from bottom of field up,
 ;fieldHeight number of times
 ld hl,fieldHeight-1 * 10 + field - 1 
 ld de,fieldHeight * 10 + field - 1
 ld b,240
 ld c,0
applyGravityOnce:
 ld a,NULL_BLOCK
 cp (hl)
 jr z,noCopyNoSwap
 ;copy block down
 ex de,hl
 cp (hl)
 jr nz,noCopy ;not a null: no copy down possible
 ;block can be copied
 inc c ;copy counter
 ld a,(de)
 ld (hl),a
 ld a,NULL_BLOCK
 ld (de),a
 
noCopy:
 ex de,hl ;swap back ptrs to field
noCopyNoSwap:
 dec de ;advance ptrs up field
 dec hl ;so, reading field backwards: dec ptrs
 djnz applyGravityOnce
 
 ld ix,rules
 bit rbitCascadeAnimation,(ix+rfExtra)
 jr z,noCascadeAnim
 push bc
 push hl
 push de
 ld ix,(drefIInfo)
 call drawObjectsNoReset
 call swapVRamPTR
 pop de
 pop hl
 pop bc
noCascadeAnim:
 xor a
 cp c
 pop bc ;for djnz
 jr z,noCopiesLeft
 djnz applyGravity
noCopiesLeft:

 call checkLineClears
 ret
 
newBlock:
 ld hl,(refPreview)
 res redrawObjBit, (hl)

 ld a,5
 ld (curX),a
 ld a,4
 ld (curY),a
 xor a
 ld (curR),a
 ld (curStatus),a
 ld (timerT),a

 ld a,LOCK_DISABLE
 ld (lockTimer),a
 
 ;something's kinda funny here.
 ;newBlockAgain:
 ;ld hl,rules
 ;bit rbitBagEnabled,(hl)
 ;call z, rand ;if bag is off, use regular random
 ;and $07
 ;cp RANDOM_NULL
 ;jr z,newBlockAgain
 ;ld hl,rules
 ;bit rbitBagEnabled,(hl)
 
 call getNextBagItem ;if bag enabled, use bag random
 
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
 ld a,4
 ld (curY),a
 xor a
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
 
 ld hl,curStatus ;last move wasn't rotate
 res csRotateTBit,(hl)
 
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
 cp fieldHeight
 jp nc, noRotationLeft
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
 
 ;check t-spin
 push hl
 ld hl,curStatus
 ld a,(curT)
 cp 5
 jr nz, noTRotateLeft
 set csRotateTBit,(hl)
noTRotateLeft:
 res csWallKicked,(hl)
 ld a,(rAttempt)
 cp 4
 jr z,notWallKickedL
 set csWallKicked,(hl)
notWallKickedL:
 pop hl
 
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
 cp fieldHeight
 jp nc, noRotationRight ;out of bounds
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
 
 push hl
 ld hl,curStatus 
 ld a,(curT)
 cp 5
 jr nz, noTRotateRight
 set csRotateTBit,(hl)
noTRotateRight:
 res csWallKicked,(hl)
 ld a,(rAttempt)
 cp 4
 jr z,notWallKickedR
 set csWallKicked,(hl)
notWallKickedR:
 pop hl

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
 
 ld hl,curStatus ;last move wasn't rotate
 res csRotateTBit,(hl)
 
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
 ld hl,curStatus ;last move wasn't rotate
 res csRotateTBit,(hl)
 
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
 cp fieldHeight-1
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
 ld hl, (drefBlocks)
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
 
;intiialize palette from data
 ld hl, (drefPalette)
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
;h = bpp id
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
 ld e,0
 ld hl,0
 add hl,de ;add 256L
 srl d
 rr e
 srl d
 rr e
 add hl,de ;add 64L: 320L total 
 pop de ;is this better than below? idk
 ;push de
 ;ld h,0
 ;ld d,0 ;save L = Y
 ;ld e,l
 ;add hl,hl ;2y
 ;add hl,hl ;4y
 ;add hl,de ;5y
 ;add hl,hl ;10y
 ;add hl,hl ;20y
 ;add hl,hl ;40y
 ;add hl,hl ;80y
 ;add hl,hl ;160y
 ;add hl,hl ;320y
 ;pop de
 add hl,de ;320Y+X
 ld de, (vramOffPtr) ;to second half of vRam, for double-buffering
 add hl,de ;320Y+x+vRam
;hl=vramptr ix=data b=sizeY c=sizeX a=pal 
putSpriteYLoop:
 push bc
 ld b,c ;size x still
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

ppbpp:
 jp putPixel1bpp
 jp putPixel2bpp
 jp putPixel4bpp
 jp putPixel8bpp
 jp put1bppHalfScale
 jp put2bppHalfScale
 jp put4bppHalfScale
 jp put8bppHalfScale

;a=pal
;ix=data ptr
;hl=vram ptr
putPixel8bpp:
 ld b,a ;b=palette
 ld a,(ix+0) ;pixel color
 cp 0 ;check if no color/transparent color
 jp z,transPixel8
 add a,b ;add color to palette ofs
 ld (hl),a
transPixel8:
 ld a,b
 inc ix ;sprite data index
 inc hl ;coords
 ret

put8bppHalfScale:
 ld e,a ;e=palette
 ld a,(ix+0) ;pixel color
 cp 0 ;check if no color/transparent color
 jp z,transPixel8h
 add a,e ;add color to palette ofs
 ld (hl),a
transPixel8h:
 inc ix ;sprite data index
 inc ix
 inc hl ;coords
 
 ld a,1
 cp b
 ld a,e
 ret nz ;return if not last x loc
 ;c is still the width of X
 ld de,0
 ld e,c ;add byte width to ix to skip next row
 add ix,de
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

put4bppHalfScale:
 ld d,a ;save palette ofs
 ld a,(ix+0)
 srl a
 srl a
 srl a
 srl a
 cp 0
 jr z, noPutPixelh
 add a,d ;add palette ofs to color
 ld (hl),a ;load nibble + palette ofs
noPutPixelh:
 inc hl ;next coord
 inc ix ;next data chunk
 
 ld a,1
 cp b
 ld a,d
 ret nz ;return if not last x loc
 ;c is still the width of X
 ld de,0
 ld e,c ;add byte width to ix to skip next row
 add ix,de
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
 
put2bppHalfScale:
 push bc
 ld d,a ;save palette ofs
 ld e,(ix+0)
 ld b,2
putPixel2h:
 xor a ;a=0
 rlc e
 rla
 rlc e
 rla ;get 2 bits from c into a
 rlc e
 rlc e ;skip next 2 bits
 cp 0
 jr z, trans2h
 add a,d ;add pal and ofs
 ld (hl),a
trans2h:
 inc hl
 djnz putPixel2h
 inc ix
 pop bc

 ld a,1
 cp b
 ld a,d

 ret nz ;return if b!=1
 ld de,0
 ld e,c
 add ix,de
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
 
put1bppHalfScale:
 push bc
 ld c,(ix+0)
 ld b,4
 ;new:
 ;c=bit data
 ;b=pixel count
putPixelBith:
 rlc c ;get bit from c
 ;if bit is zero, pixel is transparent.
 jr nc, noPutPixelBith ;skip to inc location
 ld (hl),a ;save to location
noPutPixelBith:
 inc hl ;next
 rlc c
 djnz putPixelBith
 inc ix ;done with this byte of data
 pop bc
 
 ld d,a
 ld a,1
 cp b
 ld a,d

 ret nz ;return if b!=1
 ld de,0
 ld e,c
 add ix,de
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
 
drawGame:
 ld ix,(drefIInfo)
 call drawObjectsNoReset
 call updateMino

 call swapVRamPTR
 
 ld ix,(drefIInfo)
 call drawObjects
 call updateMino 
 ret
 
;checks if mino update is needed
;if yes, draws mino
updateMino:
 ld hl, curStatus
 bit csClearLineBit, (hl)
 ret nz
 ld hl, curStatus + midOfs
 bit csClearLineBit, (hl)
 ret nz
 
 ld hl, curStatus
 bit csGarbageBit, (hl)
 ret nz
 ld hl, curStatus + midOfs
 bit csGarbageBit, (hl)
 ret nz
 
 call eraseOldMino
 call drawNewMino
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
 bit rbitHighLine,(ix+rfScore)
 jr nz, checkHighLine
 ;unneeded due to order:
 ;bit rScore,(ix+rfScore)
 ;jr checkHighScore
 
checkHighScore:
;note: top outs for score are A-OK. Only timed games need to worry about "cheesing" for low scores.
 ld hl,(highscore)
 ld de,(score)
 or a,a
 sbc hl,de
 ;carry implies highscore < score 
 ret nc ;return if no high score
 ;set high score
 ld hl,(score)
 jr setBestScore
  
checkHighLine:
 ld hl,(highscore)
 ld de,(lines)
 or a,a
 sbc hl,de
 ;carry implies highscore < score 
 ret nc ;return if no high score
 ;set high score
 ld hl,(lines)
 jr setBestScore
 
checkLowTime:
 bit rbitGameWon,(ix+rfBasic)
 ret z ;return if not set = not a win
 ;since low times can be earned by topping out rapidly, do not count these
 
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
 ld de, midData
 call NullPTRMino
 ret
 
drawNewMino:
 ld ix,(refField)
 ld de, curData
 call drawPTRMino
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
 jp 0 ;undefined
 jp drawCustom
 ret ;this one is just aesthetic
 
sharedDSO:
 jp drawSpriteObj

drawCustom:
 or a,a
 sbc hl,hl
 ld h,(ix+iDataPTRH)
 ld l,(ix+iDataPTRL)
 ld de,SSS
 add hl,de
 
 jp (hl) ;the rest is up to special drawing code
 
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
 push hl ;display coords should be [0,19] and [0,9]
 ld a,fieldHeight - 20 ;find y coord of last 20 visible rows
 add a,l ;l + ofs from top (5, if fieldHeight is 25)
 ld l,a ;y=[0-19]+[fieldHeight-20] for reading field info
 call checkBlock
 cp NULL_BLOCK
 jr z,drawNullBlock
 ld a,(hl) ;a is now curT
 ld d,a
 add a,a ;2
 ld de,0
 ld e,a
 ld hl,(drefBlocks)
 add hl,de ;find block sprite ID and pal from here
 ld a,(hl)
 ld (tSpriteID),a
 inc hl ;spritePAL
 ld a,(hl)
 ld (tSpritePAL),a
drawNullBlockReturn:
 pop hl ;use same grid coords, but operate differently
 ld e,h ;save e=x
 ld h,(ix+iDataA)
 mlt hl ;hl = 12*y implies l=12*y since y<20
 ld d,(ix+iDataA)
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
 ld h,(ix+iDataA)
 mlt hl ;hl = 12*y implies l=12*y since y<20
 ld d,(ix+iDataA)
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

 ld a,12
 add a,(ix+iDataY)
 ld l,a ;y+12 to center in hold box
 
 ld de,0
 ld d,(ix+iDataXH)
 ld e,(ix+iDataXL)
 ld h,0 ;no special settings for hold
 ld a,(holdT)
 jp drawMinoFromType
 
drawPreview:
 ;ix is data ptr
 push ix
 ld ix,rules
 bit rbitPreviewEnabled,(ix+0)
 pop ix ;restore data ptr
 ret z ;preview is disabled
 
 push ix
 ld de,0
 ld d,(ix+iDataXH)
 ld e,(ix+iDataXL)
 ld ix,SSS
 add ix,de
 res redrawObjBit,(ix+iDataType)
 call drawObject
 pop ix
 
 ld hl,0
 ld h,(ix+iDataPTRH)
 ld l,(ix+iDataPTRL)
 ld de,SSS
 add hl,de
 ;hl = ptr to extra coords
 
 ld de,randBag
 ld b,(ix+iDataW)
drawAllPreviewMinos:
 push bc
 ld a,(de) ;get preview minotype
 
 ld b,a
 inc de
 push de
 
 ld de,0
 ld e,(hl)
 inc hl
 ld d,(hl) ;de = x
 inc hl
 ld a,(hl)
 inc hl
 push hl ;save ptr to coords
 
 or a,a
 sbc hl,hl
 ld l,a ;a -> l = y
 ld h,1 << drawMinoHalf
 ld a,b ;get preview minotype again
 
 
 call drawMinoFromType
 pop hl
 pop de
 pop bc
 djnz drawAllPreviewMinos
 ret
 
blockDataPTR:
 .dl 0 
 
multABy12ToBC:
 bit 7,a
 ld bc,0
 jr z,noSignExtendBC
 dec bc
noSignExtendBC:
 ld c,a ;x 
 add a,a ;2*x
 add a,c ;3*x
 add a,a ;6*x
 add a,a ;12*x
 ld c,a ;12x
 ret

;input:
;a = type
;output:
;a = pal
;ix = sprdata ptr
;h = sprdata bpp
;(blockDataPTR)
getMinoGraphic:
 ld de,0
 add a,a
 ld e,a ;de=type*2
 ld hl,(drefBlocks)
 add hl,de ;hl points to sprite and palette info
 ld a,(hl) ;a=sprite id
 inc hl ;to palette info
 ld d,(hl) ;d=palette
  
 ld ix,(drefSprite)
 ld h,(ix) ;save bpp
 inc ix
 call getBPPFromID
 ;h=bpp id
 ;c=bpp
 
 ld b,18 ;sprite 1bpp size 12x12 sprite
 mlt bc ;144 * c / 8 -> 18 * c, c in [1,2,4,8]
 ;therefore c<256
 ld b,a
 mlt bc ;spritedatasize*spriteID
 add ix,bc ;add to sprite data: ix is spdata ptr
 
 ld b,h ;save bpp
 ld a,d ;restore palette
 ;calculate block data ptr
 ld d,0
 ld hl,blockData
 add hl,de ;2
 add hl,de ;4
 add hl,de ;6
 add hl,de ;8x type + blockdata
 ld (blockDataPTR),hl
 ld h,b
 
 ;h = bpp
 ;a = palette
 ;ix = sprdata
 ;(blockDataPTR)
 ret
 
;input:
;de = x
;l = y

drawMinoBlock:
 ld b,12
smcDMB:
 ld h,0
 ld ix,0
 ld a,0
 ld c,0
 call drawSpriteCustomBPP
 ret
smcDMB_h = smcDMB + 1
smcDMB_ix = smcDMB + 4
smcDMB_a = smcDMB + 8
smcDMB_c = smcDMB + 10
 
;inputs:
;de= x
;l = y
;h = settings
;a = type
drawMinoFromType:
 push de
 push hl
 call getMinoGraphic
 ld (smcDMB_ix),ix
 ld (smcDMB_a),a
 ld a,h
 pop hl
 bit drawMinoHalf,h
 jr z,notHalfBPP
 set 2,a ;set "half scale" bit
notHalfBPP:
 ld (smcDMB_h),a
 res 2,a
 ld d,h ;save settings to d
 ld h,a
 
 ld c,12
 ld b,h
 inc b
divideByBPPdmft:
 rrc c
 djnz divideByBPPdmft
 
 ld a,c
 ld (smcDMB_c),a ;pixel width
 ld h,d ;restore setting
 pop de
 xor a
 inc a
 ld (smcDMBs_xOfs),a ;1 = grid x offset 
 ld (smcDMBs_yOfs),a ;also 1 = y offset 
 
 ;at this point, graphical data has been set
 ;except:
 ;h = settings
 ;l = y
 ;de= x
 ;this will be modifed in loop, using blockDataPTR
 jr drawMinoBlocksLoopBegin
 
;inputs:
;ix= obj data ptr
;de= curdata ptr
;h = settings
drawMinoFromData:
 push ix
 ex de,hl ;hl = curdata ptr, d=settings
 ld a,(hl) ;get x grid ofs
 ld (smcDMBs_xOfs),a
 inc hl
 
 ld a,(hl) ;get y grid ofs
 sub fieldHeight - 20
 ld (smcDMBs_yOfs),a
 inc hl ;past y
 
 ;past curR,T,Status
 inc hl
 ld a,(hl) ;type
 inc hl
 inc hl

 push hl ;blockDataPTR
 push de ;d=settings
 call getMinoGraphic
 pop de ;d=settings
 
 ld (smcDMB_ix),ix
 ld (smcDMB_a),a
 
 bit drawMinoHalf,d
 jr z,noHalfScale
 set 2,h
noHalfScale:
 ld a,h 
 ld (smcDMB_h),a
 
 res 2,a
 ld h,a
 
 ld c,12
 ld b,h
 inc b
divideByBPPdmfd:
 rrc c
 djnz divideByBPPdmfd
 bit drawMinoHalf,d
 jr z,noHalfAdjust
 rrc c
noHalfAdjust:
 
 ld a,c
 ld (smcDMB_c),a
 
 pop hl
 ld (blockDataPTR),hl
 
 ex de,hl ;settings in h
 pop ix 
 ld l,(ix+iDataY)
 ld de,0
 ld d,(ix+iDataXH)
 ld e,(ix+iDataXL)
 
 jr drawMinoBlocksLoopBegin
 
drawMinoBlocksLoopBegin:
 bit drawMinoErase,h
 jr z,noErase
 xor a
 ld (smcDMB_a),a
 ld ix,(drefSprite)
 inc ix ;past bpp
 ld (smcDMB_ix),ix
noErase:
 bit drawMinoDark,h
 jr z,noDark
 ld ix,smcDMB_a
 ld a,32
 add a,(ix)
 ld (ix),a
noDark:
 
 ld b,4
drawMinoBlocksLoop:
 push bc
 push de
 push hl
 
 ld bc,(blockDataPTR)
 ld a,(bc)
 inc bc
 ld (blockDataPTR),bc

smcDMBs_xOfs = $ + 1
 add a,0
 cp 10 ;if it goes past 10, it's out-of-field
 jr nc, skipDMB
 
 call multABy12ToBC
 bit drawMinoHalf,h
 jr z,noHalfDMBx
 srl c ;half multiplied value
noHalfDMBx:
 ;12x -> bc (or 6x if half-scale)
 ex de,hl ;hl=x
 add hl,bc ;add xofs
 ex de,hl ;hl=settings/y again de=x
 
 ld bc,(blockDataPTR)
 ld a,(bc)
 inc bc
 ld (blockDataPTR),bc

smcDMBs_yOfs = $ + 1
 add a,0
 cp 20 ;if it goes past 20, it's out-of-field
 jr nc, skipDMB
 
 call multABy12ToBC
 bit drawMinoHalf,h
 jr z,noHalfDMBy
 srl a ;half multiplied value
noHalfDMBy:
 ;12y->bc
 add a,l ;add ofs to y ofs
 ld l,a
 
 ;de = x
 ;l = y
 
 call drawMinoBlock
skipDMB:
 pop hl
 pop de
 pop bc
 djnz drawMinoBlocksLoop
 ret
 
;requires ix as obj data ptr
;de as setDataFromPTR ptr to curdata etc
;also, nice
nullPTRMino:
 ld h,1
 call drawMinoFromData
 ret
 
;ix = obj data ptr
;de = setDataFromPTR ptr
drawPTRMino:
 ld h,0
 call drawMinoFromData
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
 or a,a
 ld de,SSS
 sbc hl,de ;hl=ofs from SSS
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
 
CETrisSavVar:
 .db AppVarObj, "CETrisSV", 0
 
loadSaveVar:
 ld hl, CETrisSavVar
 call _Mov9ToOP1
 call _ChkFindSym
 jr c, createSave
 call _ChkInRam
 ret z
 ld hl, CETrisSavVar
 call _Mov9ToOP1
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
 ld (theme),a
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
 
CETrisDataVar:
.db AppVarObj, "CETrisDT",0
 
loadData:
 ld hl, CETrisDataVar
 call _Mov9ToOP1
 call _ChkFindSym
 jp c, error
 push de
 call _ChkInRam ;proper check for variable location
 pop de
 ret z
 ;unarchive and move to ram
 ld hl, CETrisDataVar
 call _Mov9ToOP1
 call _Arc_Unarc
 ld hl, CETrisDataVar
 call _Mov9ToOP1
 call _ChkFindSym
 ;de=data ptr
 ret
 
rearchiveData:
 ld hl, CETrisDataVar
 call _Mov9ToOP1
 call _Arc_Unarc
 ret
 
;copies data from SSSCopiedData to SSS
initData:
 call loadData
 ex de,hl ;hl = data ptr
 ld bc,0 
 ld c,(hl) ;size of data
 inc hl
 ld b,(hl) ;size high byte
 inc hl
 ld de,SSS
 ldir
 
 call rearchiveData
 
 ld hl,PSS768CopiedData
 ld de,blockData
 ld bc,PSS768CopiedDataEnd - PSS768CopiedData
 ldir
 
 ;copy to PSS2048
 ld hl,menuJumps
 ld de,menuJumpTable
 ld bc,menuJumpsEnd - menuJumps
 ldir
 
 ld a,30
 ld (lockDelay),a
 ret
 
;program data
;note: doesn't really change much
pointsPerLine:
 .db 0,1,03,05,08,12,16,20,24,28,32,36,40
pointsPerTMini:
 .db 1,2
pointsPerTLine:
 .db 4,8,12,16,20,25,30,35,40,45,50,55,60
 ;24+ is impossible... jk, cascade mode

;frames until block drops one row
;stored 8.16 fixed point
speedCurve:
 .dl 1092, 1377, 1768, 2311, 3075
 .dl 4169, 5759, 8107, 11634, 17026
 .dl 25416, 38709, 60169, 95483, 154742
 .dl 256187, 433425, 749597, 1325716, 2398490

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
 
;this has never been used properly because it wasn't worth it, but it might be used later for "accuracy purposes"
kicksO:
 .db  0, 0
 .db  0, 1
 .db -1, 1
 .db -1, 0
 .db  0, 0

;this is needed to give important menu jumps 
;fixed addresses to call from the data file, 
;which doesn't have access to the main program's 
;data. That way, menus can be designed separately
;from the program and allow for more customization.
;this will be copied to PSS+2048
menuJumps:
 jp mainMenu				;return to main menu
 jp initGame				;start game
 jp exit					;exit program
 jp activeMenu				;"runs" a menu object
 jp getStringInList			;gets [a] in menu/list
 jp getStringPTRSelection	;gets selected item
 jp setNumber				;select 8bit number
 jp drawObject				;draw given object
 jp checkKey				;keypress to [a]
 jp swapVRamPTR				;exactly what it says
menuJumpsEnd: