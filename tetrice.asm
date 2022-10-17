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
 
 ld hl,(drefFont)
 ld (smcFontDataPtr),hl
 ld hl,(drefPalette)
 ld (smcPaletteDataPtr),hl
 
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
 ld (ix+rfBasic), rMarathon ;game/component rules
 ld (ix+rfExtra), rNull ;no extra rules
 ld (ix+rfWin), rLines ;win method
 ld (ix+rfScore),rScore ;scoring method
 
 xor a
 ld (ix+rMode),a
 ld (ix+rRGT),a
 ld (ix+rRGD),a
 ld (ix+rLCW),150
 
 inc a ;a=1
 ld (level),a ;starting level is by default one
 ret 

;must have rules+rMode already set
setHighScore:
 ld a,(rules+rMode)
 ld ix,(saveDataPTR)
 ld de,0
 add a,a
 add a,a
 ld e,a
 add ix,de ;add 4
 add ix,de ;add 8
 ld hl,(ix+savHighScore) ;hl=score from save
 ld (highscore),hl
 ret
 
mainMenu:
 ld ix,(drefMenu)
 jp activeMenu
 
initGame:
 ld ix,rules
 set rbitGameCont, (ix+rfBasic) ;game is going
 res rbitGameWon, (ix+rfBasic) ;game not won
 
 xor a
 ld (lines),a
 ld (linesToNextLevel),a
 ld (clearTimer),a
 ld (spawnTimer),a

 ld hl,0
 ld (score),hl ;don't keep old score/time!
 bit rbitCountdown, (ix+rfWin)
 jr nz, noResetTimer
 ld (globaltimer),hl
noResetTimer:
 call setHighScore
 call setDelay
 
 ld a,NULL_BLOCK
 ld (holdT),a
 
 call initBag ;also initializes random I guess?
 call initField ;depends on RNG being seeded by initbag, which init's rng system
 
 call newBlock
 
 ;first draw to set up screen
 call clearLCD
 ld ix,(drefIInfo)
 call resetAllNeeded
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
 
 jp handleDelays
noDelays:

 call update
 call userUpdate
 
skipGameUpdate:
 call drawGame
 
 ld hl,rules + rfWin
 bit rbitCountdown, (hl)
 call nz, timerDown ;timer decreases
 call z, timerUp ;timer increases
 
 ld hl, (refTimer) ;time updates every frame
 res redrawObjBit, (hl)
 ld hl,(refScore)
 res redrawObjBit, (hl)
 
 ld ix,rules
 bit rBitGameCont, (ix+rfBasic)
 jr nz, game ;jump if nz: bit is 1, game is going
 jp gameEndInit

handleDelays:
 ld a,(clearTimer)
 or a,a
 jr z, noClearTimer
 dec a
 ld (clearTimer),a
 jr waitForClear
noClearTimer:

 ld a,(spawnTimer)
 or a,a
 jr z, noSpawnTimer
 dec a
 ld (spawnTimer),a
 jr waitForSpawn
noSpawnTimer:
 jr noDelays

waitForClear:
waitForSpawn:
;waiting for something or other
;check for instant actions and DAS update in ARE 
 call userInstantUpdate 
 jr skipGameUpdate

timerDown:
 ld hl,(globalTimer)
 dec hl
 ld (globalTimer),hl
 xor a
 or h
 or l
 ret nz ;if hl nonzero, return (also prevents timerUp from being called)
 ld hl, rules + rfBasic
 res rbitGameCont, (hl) ;game ends when reaching zero
 set rbitGameWon, (hl) ;game is won if you survived to here
 or 1 ;resets zero so timerUp isn't called
 ret

timerUp:
 ld hl,(globalTimer)
 inc hl
 ld (globalTimer),hl
 ret

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
 ld a,7 ;garbage block?
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
 ld hl,DrawObjectsNoReset
 ld (smcDrawObjectsType),hl
 ret

shiftOldData:
 ;copy old mino data
 ;to use for clearing old draws
 ld hl, midData
 ld de, oldData
 ld bc, curDataSize
 ldir
 ;note: can probably combine these into one copy
 ld hl, curData
 ld de, midData
 ld bc, curDataSize
 ldir
 
 ld hl, midTempData
 ld de, oldTempData
 ld bc, curDataSize
 ldir 
 
 ld hl, tempData
 ld de, midTempData
 ld bc, curDataSize
 ldir 
 ret

userInstantUpdate:
 ld ix,buttonLeft
 call checkKeyDown 
 ld ix,buttonRight
 call checkKeyDown 
;results discarded, used to update button timers for DAS
 
 xor a
 ld (queuedAction),a ;clear any possibly queued moves
 ld ix,buttonHold
 call checkKeyHeld
 jr nc, skipQueueHold
 ld hl,queuedAction
 set qaHold,(hl)
skipQueueHold:
;this happened naturall because of button timings
; ld ix,buttonRotateLeft
; call checkKeyHeld
; jr nc, skipQueueRL
; ld hl,queuedAction
; set qaRotateLeft,(hl)
;skipQueueRL: 
;
; ld ix,buttonRotateRight
; call checkKeyHeld
; jr nc, skipQueueRR
; ld hl,queuedAction
; set qaRotateRight,(hl)
;skipQueueRR:
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
;turns out unneeded due to button timing system
;ld a,(queuedAction)
;bit qaRotateLeft, a
;call nz, checkRotationLeft
 
 ld ix,buttonRotateRight
 call checkKeyDown
 call c, checkRotationRight
;ld a,(queuedAction)
;bit qaRotateRight, a
;call nz, checkRotationRight
 
 ld ix,buttonHold
 call checkKeyDown
 call c, hold
 ld a,(queuedAction)
 bit qaHold, a
 call nz, hold
 
 ld ix,buttonPause
 call checkKeyDown
 call c, pause
 
 xor a
 ld (queuedAction),a
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
 call resetAllNeeded
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

 ld hl, curStatus
 bit csUsedHold,(hl)
 ret nz ;hold already used once
 
 ;tell draw system to redraw HOLD
 ld ix,(refHold)
 res redrawObjBit, (ix+iDataType)
 ld hl, drawObjectsNoReset
 ld (smcDrawObjectsType),hl
 
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
 ld hl,curStatus
 set csUsedHold,(hl)
 ret
firstHold:
 ld a,(curT)
 ld (holdT),a
 
 call newBlock ;geneate a new block: nothing to load
 ld hl,curStatus
 set csUsedHold,(hl)
 ret
 
update:
 ld hl, curStatus + midOfs ;last frame status
 bit csNewBlockBit, (hl) ;request new block
 jr z,noNewBlock
 call newBlock ;if set, create block
 ;also requires update hold, if held needs to change
 ld ix,rules
 bit rbitHoldEnabled, (ix+rfBasic)
 jr z, noDrawHold
 ld hl,(refHold)
 res redrawObjBit, (hl)
noDrawHold:
noNewBlock:
 
 bit rbitLinesClear, (ix+rfWin)
 jr z, skipEndCheck
 ;check if lines cleared is less than lines needed
 ld a,(lines)
; ld hl, rules+rLCW
 cp (ix+rLCW) ;nc (hl)<=a ;c (hl)>a
 jr c, skipEndCheck
 ;game has ended, player won
 ;ld hl, rules
 res rbitGameCont, (ix+rfBasic)
 set rbitGameWon, (ix+rfBasic) ;win by lines clear
 
skipEndCheck:
 
 xor a
; ld (PSS+444),a
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
; ld a,(PSS+444)
; inc a
; ld (PSS+444),a
 ld (timerT),hl
 ld ix,curData
 call checkBlockDown
 jr checkGravityLoop ;must check for more than 1 row/frame if not locking
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
 ld ix,curData
 call checkBlockDown
 jr hardDropLoop
hdrop:
 ld a,1
 ld (lockTimer),a
 jr dropReturn
 
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
 ld ix,curData
 call checkBlockDown
 jp dropReturn
 
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
 ;delay spawn by some frames
 ld a,(spawnDelay)
 ld (spawnTimer),a


 ld b,8
lockAnimLoop:
 push bc
 ; probably erases a lot it doesn't need to
 ld de, midOfs + curData
 ld ix,(refField)
 ld h,1<<drawMinoErase
 push bc
 call eraseOldMino
 pop bc
 
 ld ix,(refField)
 ld de, curData
 ld h,0
 bit 1,b
 jr z, skipDark
 ld h,1<<drawMinoDark
skipDark:
 call drawMinoObject ;likely not actually visible anyways
 call swapVRamPTR
 pop bc
 djnz lockAnimLoop
 ret

lockAnimStart:
 ld ix,(refField)
 ld de, curData
 ld h,1<<drawMinoDark
 call drawMinoObject ;likely not actually visible anyways
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
 
 ld hl, drawObjectsNoReset
 ld (smcDrawObjectsType), hl
 
 ld hl, curStatus
 set csClearLineBit, (hl) ;clear bit 

 ld ix,rules
 bit rbitCascadeGravity,(ix+rfExtra)
 call nz,cascadeBlocks
 
noLinesSpin: ;needs to update score for spins etc.
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
 cp 21
 jr c, noLowerLevelScore
 ld a,20
noLowerLevelScore:
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
 add hl,de ;score+=100a (or 150a)
 ld (score),hl
 
 ;increment level for every 10 lines cleared
 ld a,(linesToNextLevel)
 sub 10 ;equiv flags to cp 10 and saves sub after
 jr c, noLevelUp
 ld (linesToNextLevel),a
 ld hl, (level)
 inc hl
 ld (level),hl
 
 call setDelay ;set delay changes for higher levels
noLevelUp:
noLinesClear:
;all of the above do not update when not cleared.
 ret

setDelay:
 ld a,(level)
 ; calculate Clear delay decrease
 ld hl,clearDelay
 ld (hl),35
 cp 21
 jr c, noClearDelayDecrease
 ld (hl),0
 cp 51
 jr nc, noClearDelayDecrease
 ;20 < a < 50
 neg
 add a,50
 ld (hl),a ;clear delay down by one per level past 20
 ;this caps at about 10 frames delay for the base setup
 ;which is entirely the fault of the rendering code and can't be easily lowered
noClearDelayDecrease:
 ld a,(level)
 ld hl, delayCurveEnd
;applies first delay possible, reading from the back
untilFindLevelOrNone:
 cp (hl)
 jr nc, applyNewDelay ; a >= (hl), so apply the delay
 dec hl
 dec hl
 dec hl
 dec hl
 jr untilFindLevelOrNone

applyNewDelay:
 inc hl
 ld a,(hl)
 ld (spawnDelay),a
 inc hl
 ld a,(hl)
 ld (lockDelay),a 
 inc hl
 ld a,(hl)
 ld (buttonLeft+buttonTimeStart),a
 ld (buttonRight+buttonTimeStart),a
 
noDelayDecrease:
 ret

 
updateB2B:
 ld hl,lineClearInfo

 ;keep set until explicitly reset
 bit lcBackToBack,(hl)
 jr z,noPrevB2B
 set lcPreviousB2B,(hl)
 res lcBackToBack,(hl) ;make sure this frame is checked
noPrevB2B:

 ld a,(linesClear)
 or a,a
 jr z,B2BDone ;unaffected by no lines cleared

 ;check for potential back-to-back status on this line clear
 bit lcTSpin,(hl)
 jr z,checkTetrisB2B ;no t spin = check lines amt
 
 set lcBackToBack,(hl)
 jr B2BDone ;set from T spin and line clear

checkTetrisB2B:
 set lcBackToBack,(hl) ;set if tetris or better
 ld a,(linesClear)
 cp 4
 jr nc,B2BDone ;reset combo if not tetris
 res lcBackToBack,(hl) ;default unset
 res lcPreviousB2B,(hl) ;needs 2 
B2BDone:
 res lcTSpin,(hl) ;don't keep next for next block
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
 ld hl, rules
 bit rbitPreviewEnabled, (hl)
 jr z, noRedrawPreview 
 ld hl,(refPreview)
 res redrawObjBit, (hl)
 ld hl, DrawObjectsNoReset
 ld (smcDrawObjectsType), hl
noRedrawPreview:
 
 ld hl,rules
 bit rbitBagEnabled,(hl)
 jr nz, bagEnabled
 
newBlockAgain:
 call rand
 ld a,e
 and $07
 cp RANDOM_NULL
 jr z, newBlockAgain
 jr noBag
bagEnabled:
 call getNextBagItem
noBag:
 ld (curT),a

createBlockShared:
 ld a,4
 ld (curX),a
 ld (curY),a
 xor a
 ld (curR),a
 ld (curStatus),a ;important: reset all status bits
 ld (timerT),a

 ld a,LOCK_DISABLE
 ld (lockTimer),a
 
 ld ix,blockData
 ld d,a
 add a,a
 add a,a
 add a,a
 ld de,0
 ld e,a
 add ix, de
 ld a,(curT) ;ensure copied blockdata is CORRECT?
 call copyBlockData

 ld ix,curData 
 call checkBlockDownOK
 or a,a
 call z,gameEnd
 ;if block check fails immediately, it's a top out
 ret
 
;a=block type
determinedBlock:
 ld (curT),a
 jr createBlockShared
 
;inputs:
;a = $44 -> neg x
;b = $44 -> neg y
;e = $3c or $3d: inc a or dec a
;hl = 10 or -10? (direction to travel kick table)
ModifyRotate:
;ld a,$44 ;neg
 ld (smcCheckRotateNegX),a
 ld (smcRotateNegX),a
 ld a,b
;ld a,$3c ;tst a (used as a 2-byte no-op)
 ld (smcCheckRotateNegY),a
 ld (smcRotateNegY),a
;ld hl,-10
 ld a,e
 ld (smcDecInc),a
 ld (smcCheckCalcRXY),hl
 ld (smcNoRotateRXY),hl
 ret

checkRotationLeft:
 ld a,(curR)
 or a,a
 jr nz,skipAdjust4
 ld a,4
skipAdjust4:
 ld (curR),a
; modify a bunch of different points that are similar but not identical 
 ld a,$44
 ld b,$3c
 ld e,$3d
 ld hl,-10
 call ModifyRotate
 jr checkRotation

checkRotationRight:
; modify a bunch of different points that are similar but not identical 
 ld a,$3c ;tst a (essentially a two-byte NOP here)
 ld b,$44
 ld e,$3c ;inc a
 ld hl,10
 call ModifyRotate
 jr checkRotation

checkRotation:
 ld a,(curT)
 ld hl, kicksNormal
 cp 1 ; check I piece
 jr nz, notIKicks
 ld hl, kicksI
notIKicks:
 cp 4 ; check O piece
 ret z
 push hl ;save table address
 
 call getTableOffset ;de=offset
 pop hl ;restore table address
 add hl,de ;offset + table = starting rotation kick

smcCheckCalcRXY=$+1
 ld de,-10
 call calculateRXY ;hl is up two now, rxy stored
 push hl ;at next offset already
 
 xor a
 ld (rAttempt),a ;there are currently 0 rotation attempts.
 
 ld hl, curBlock
 ld b,4 ;TODO make variable
checkBlocksRotate:
 ld (rotationTempBC),bc
 ;will be <x,y> to <-y,x> LEFT or <y,-x> RIGHT
 ;get new y
 ld a,(hl)
smcCheckRotateNegX=$+1
 neg
 ld e,a
 ld a,(rY)
 add a,e
 cp fieldHeight ;TODO: make variable
 jp nc, noRotation
 ld e,a
 
 ;get new x
 inc hl
 ld a,(hl)
smcCheckRotateNegY=$+1
 neg
 ld d,a
 ld a,(rX)
 add a,d
 cp 10 ;TODO: make variable
 jr nc, noRotation
 ld d,a
 
 ld (rotationTempHL),hl
 push de
 pop hl ;d,e -> h,l are new rotated coordinates
 call checkBlock
 cp NULL_BLOCK
 jp nz,noRotation ; if a block is found, can't rotate this way
 ld hl,(rotationTempHL)
 inc hl ;hl now points to next x offset
 ld bc,(rotationTempBC)
 djnz checkBlocksRotate
;rotation is a go
 ld b,4
 ld hl, curBlock
rotateBlocks:
 push bc
 ld a,(hl)
smcRotateNegX=$+1
 neg
 ld e,a
 inc hl
 ld a,(hl)
smcRotateNegY=$+1
 neg
 ld d,a
 dec hl ;back to X location
 ld (hl),d
 inc hl
 ld (hl),e ;blocks have rotated: now do the next one
 inc hl
 pop bc
 djnz rotateBlocks
 pop hl ;next kick offset (unneeded here)
 
 ld a,(curR)
smcDecInc:
 inc a ;right
 and $03
 ld (curR),a
 
 ld a,(rX)
 ld (curX),a
 ld a,(rY) ;save the location of the successful turn.
 ld (curY),a
 
 ;check t-spin
 ld hl,curStatus
 ld a,(curT)
 cp 6 ; T block
 jr nz, noTRotate
 set csRotateTBit,(hl)
noTRotate:
 res csWallKicked,(hl)
 ld a,(rAttempt)
 cp 4
 jr z,notWallKicked
 set csWallKicked,(hl)
notWallKicked:
 
 ld ix,curData
 ;there was a rotate: set lock timer if needed
 call setLockTimerIfGrounded
 ret

noRotation:
 pop hl ;get offset
 
smcNoRotateRXY=$+1
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
 jp nz, checkBlocksRotate

 pop hl ;unneeded.
 call setLockTimerIfGrounded 
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

checkMoveLeft:
 ld e,0
 ld d,-1
 jr checkMove 

checkMoveRight:
 ld e,9
 ld d,1

checkMove:
 ld a,(curX)
 ld hl, curBlock
 ld b,4
checkBlocksToSide:
 ld c, (hl) ; c=xOfs
 ld a,(curX)
 add a,c ;a=x+xOfs
 cp e
 jr z,blockTooFar
 push bc
 push de ;contains d=direction e=edge coordinate
;check if block will collide
 ld c,(hl)
 inc hl
 ld b,(hl) ;(c,b) = (ofsx, ofsy)
 inc hl
 push hl ;blockdata at next coord already
 ld a,(curX)
 add a,c
 add a,d
 ld h,a
 ld a,(curY)
 add a,b
 ld l,a
 call checkBlock
 cp NULL_BLOCK
 pop hl ;restore and inc 2 block data ptr
 pop de
 pop bc
 jp nz, blockTooFar ;collision with block
 djnz checkBlocksToSide
 
 ;here: no blocks too far left...
 ld a,(curX)
 add a,d
 ld (curX),a
 
 ld hl,curStatus ;last move wasn't rotate
 res csRotateTBit,(hl)
 
 ;check if block needs lock
blockTooFar:
 call setLockTimerIfGrounded
 ret

;checks if the block CAN move down AND moves the block if it can.
;note: affects lock timer
;ix = curData
checkBlockDown:
 push ix
 call checkBlockDownOK
 pop ix
 or a,a
 ret z ;check block down failed: no decrement
 inc (ix+curYOfs)
 ld hl,curStatus ;last move wasn't rotate
 res csRotateTBit,(hl)

 call setLockTimerIfGrounded
 ret

;checks if the block CAN move down without actually moving it
;note: affects lock timer
;input: ix = curdata struct
checkBlockDownOK:
 lea hl, ix+curBlockOfs+1 ;ptr to y coord of curdata
 ld b,4 ;TODO: make flexible
checkBlocksInDown:
 ld a, (ix+curYOfs)
 add a,(hl)
 cp fieldHeight-1
 jr z, blockTooFarDown
 push bc
 push hl
 ld c,(hl)
 dec hl
; ld b,(hl) ;(b,c) = (ofsx, ofsy)
 ld a,(ix+curXOfs)
 add a,(hl)
 ld h,a
 ld a,(ix+curYOfs)
 add a,c
 ld l,a
 inc l ; +1 to y coord
 call checkBlock
 cp NULL_BLOCK
 pop hl
 pop bc
 jr nz,blockTooFarDown ;block is hit
 inc hl
 inc hl ;to next coord pair
 djnz checkBlocksInDown
 ld a,1 ;success
 ret

blockTooFarDown:
 xor a
 ret


;sets the lock timer if block is grounded
setLockTimerIfGrounded:
 ld a,LOCK_DISABLE
 ld (lockTimer),a
 ld ix, curData
 call checkBlockDownOK
 or a,a
 ret nz ;success -> no lock timer
 ;fail: fallthrough and set lock timer
 
;sets the lock timer if lock was previously disabled
setLockTimer:
 ld a,(lockTimer)
 cp LOCK_DISABLE
 ret nz
;sets the lock timer to the lock delay
forceLockTimer:
 ld a,(lockDelay)
 cp 2
 jr nc,nonzeroDelay
 ld a,2 ;lock delay 0 and 1 are infinite, cool
nonzeroDelay:
 ld (lockTimer),a
 ret
 
;inputs: 
;h = x
;l = y
;returns: a=block  hl=location
;destroy: de=field
;TODO: variable field width?
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
 ld hl, blockData
 dec a
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
 ret

randbag = minoTypes + bag1Ofs
randbag2 = minoTypes + bag2Ofs
RANDOM_NULL = 0 ;empty slot

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
 
 
drawGame:
 ld ix,(drefIInfo)
smcDrawObjectsType=$+1
 call drawObjects ; NoReset
 call updateMino
 ld hl, curStatus
 bit csLockedBit, (hl)
;call nz, lockAnimStart

 call swapVRamPTR

 ld hl,drawObjects
 ld (smcDrawObjectsType),hl
 
; ld ix,(drefIInfo)
; call drawObjects
; call updateMino 
 ret

; resets all except things that should be disabled for this mode
resetAllNeeded:
 ld ix,(drefIInfo)
 call resetAllDrawCheck 
 
 ld hl, rules
 bit rbitHoldEnabled, (hl)
 jr nz,holdDraw
 ld ix, (refHold)
 set redrawObjBit, (ix)
holdDraw:
 bit rbitPreviewEnabled, (hl)
 jr nz,previewDraw
 ld ix, (refPreview)
 set redrawObjBit, (ix)
previewDraw:
 ret

;checks if mino update is needed
;if yes, draws mino
updateMino:
 ;must be done before drawGhostMino to not keep old data in temp
 ld hl, curdata
 ld de, tempData
 ld bc, curDataSize
 ldir

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
 call drawGhostMino ;also erases old ghost
 call drawNewMino
 ret
 
gameEndInit:
 call checkBest

 ld ix,(drefIInfo)
 call resetAllNeeded
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
 ld hl, curStatus + oldOfs
 bit csLockedBit, (hl)
 ret nz ;don't clear a locked mino
;bit csClearLineBit, (hl)
;ret nz ;don't erase a mino that cleared lines

 ld ix,(refField)
 ld de, oldData
 call NullPTRMino
 ret
 
drawNewMino:
 ld ix,(refField)
 ld hl,curStatus + midOfs
 bit csLockedBit, (hl)
 ld de, midData
 ld h,0
 call nz, drawMinoObject
 
 ld ix,(refField)
 ld de, curData
 ld h,0
 call drawMinoObject ;likely not actually visible anyways
 ret

drawGhostMino:
 ld hl, rules + rfBasic
 bit rbitGhostEnabled, (hl)
 ret z ;ghost is disabled
 
 ld hl, curStatus + oldOfs ;can't use oldTempData due to missing changes
 bit csLockedBit, (hl)
 jr nz,skipEraseGhost ;only erase not locked blocks
;bit csClearLineBit, (hl)
;jr nz,skipEraseGhost ;don't erase a mino that cleared lines

 ld ix, (refField)
 ld de, oldTempData
 call nullPTRMino 
skipEraseGhost: 

 ld ix, tempData

lowerGhost:
 push ix
 call checkBlockDownOK
 pop ix
 or a,a
 jr z,doneLowering ;check block down failed: no decrement
 inc (ix+curYOfs) ;lower block
 jr lowerGhost
doneLowering:
 
 ld ix, (refField)
 ld de, tempData
 ld h, 1<<drawMinoDark
 call drawMinoObject
 ret

 
;inputs:
;a=type
;ix=map data
;return:
;hl=blockdata
;destroys:
;de
getMinoBlockData:
 ld de,blockData
 or a,a
 sbc hl,hl
 ld l,a
 add hl,hl ;2
 add hl,hl ;4
 add hl,hl ;8*type 
 add hl,de ;ptr to blockdata
 ret

;inputs:
;ix= map data ptr
;de= x
;l = y
;h = settings
;a = type
drawMinoFromType:
;set coordinates of mino
 ld bc,0
 ld c,l
 ld (minoBlockBaseY),bc
 ld (minoBlockBaseX),de
 ex de,hl
;d=setting
 push de
 dec a
 call getMinoBlockData
 inc a ;used as palette 
 pop de 
 push hl
 push af
 ; requires ix, d
 call drawMinoWriteC
 pop af ;a = type
 pop hl ;hl = blockdata
;ix = map data ptr
;d = settings
 jp drawMinoManualBlockData
 
; Key:
; [M] - modified by drawMino once
; [L] - modified by drawMino in loop
; [C] - constant (could be optional)
minoBlockObj:
 .db typeSprite2bpp ;[C] set from sprite data
 .dw 0 ;[L] x coordinate (px)
 .db 0 ;[L] y coordinate (px)
 .db 0 ;[M] palette
 .dl 0 ;[M] pointer to block sprite data
 .db 3, 12 ;[C] width (bytes), [C] height (pixels)
minoBlockBaseX:
 .dl 0 ;[M] x coordinate of mino (pixels)
minoBlockBaseY:
 .dl 0 ;[M] y coord (pixels)

;inputs:
;ix = obj data ptr (map/field data)
;de = curdata ptr
;h = settings 
drawMinoObject:
 ex de,hl;d=settings
 push hl ;hl=curdata
 call drawMinoWriteC
 pop hl
 call drawMinoWriteBaseXY
 jp drawMinoWriteM
 
;write to minoBlockObj all [C] values
;requires: ix, d
;preserves d
drawMinoWriteC:
 ld hl,(ix+iExtTile)
 ld a,(hl)
 bit drawMinoHalf, d
 jr z,notDrawMinoHalf
 or spHalf ; set the half-scale bit of the bpp setting 
notDrawMinoHalf:
 ld (minoBlockObj),a
 and $03
 ld b,a
 ld a,(ix+iDataA)
 call getBlockSizeBytes
 ld hl,minoBlockObj+iDataW
 ld (hl),a ;size (bytes)
 inc hl
 bit drawMinoHalf, d
 jr z,notDrawMinoHalfC
;srl c
notDrawMinoHalfC:
 ld (hl),c ;size (pixels)
 ret

;write minoBlockBase coordinates [M]
;requires: ix=map obj, hl=curdata
drawMinoWriteBaseXY:
 push hl ; will need it again
;ld c,(ix+iDataA) ;tile size (px) (kept from above)
 ld b,(hl) ; curX (tile)
 mlt bc ; curX (px)
 or a,a
 sbc hl,hl
 ld l,(ix+iDataXL)
 add hl,bc
 ld (minoBlockBaseX),hl ; fieldX + curX (pixels)
 pop hl
 inc hl ;advance to curY
 push hl ;curData+1 ptr (save for later)
 ld a,(hl)
 add a,(ix+iDataH)
 sub fieldHeight
 ld b,a ; curY - fieldHeight + fieldDrawHeight (tiles)
 ld c,(ix+iDataA)
 call multiplyBCSigned
 or a,a
 sbc hl,hl
 ld l,(ix+iDataY)
 add hl,bc
 ld (minoBlockBaseY),hl ; fieldY + curY (pixels)
 pop hl ;curY
 ret
 
;write minoBlockObj [M] values
;requires: ix, hl=curY, d
drawMinoWriteM:
 inc hl ;curR
 inc hl ;curT (mino type)
 ld a,(hl)
 inc hl ;curStatus
 inc hl ;curBlock

;requires: ix, hl=blockdata, a=type, d=settings
drawMinoManualBlockData:
 push hl ;now points to blockData
 ;check if draw null blocks instead
 bit drawMinoErase, d
 jr z,notErase
 xor a ; a=0 -> null block
notErase:
 ld hl,(ix+iExtTileset)
 ld bc,0
 ld c,a
 add hl,bc
 add hl,bc ;pointer to sprite/palette pair
 ld e,(hl) ;sprite index
 inc hl
 ld a,(hl) ;palette
 ld (minoBlockObj+iDataA),a
 ld c,(ix+iDataA) ;height (px)
 ld a,(minoBlockObj+iDataW) ;width (bytes)
 ld b,a
 mlt bc ;single sprite size (bytes, assume <256)
 ld b,e
 mlt bc ;multiply by index
 ld hl,(ix+iExtTile)
 inc hl
 add hl,bc ;ptr to sprite data
 ld (minoBlockObj+iDataPTR),hl
 pop hl
 
 bit drawMinoDark, d
 jr z,notDark
 ld a,(minoBlockObj+iDataA) ;TODO: make flexible? use bc?
;Alternate TODO: set darkBit, (ix+iDataA) ;?
 add a,32
 ld (minoBlockObj+iDataA),a
notDark:
 ld b,4 ;number of blocks (TODO make flexible)
 ld c,(ix+iDataA)
 bit drawMinoHalf, d
 jr z,notHalfMino
 srl c
notHalfMino:

;ix = map data ptr
;hl = blockdata ptr
;d = settings
;b = number of blocks in blockdata
;c = size of block (pixels)
;at this point, we have set the constants of minoBlockObj and BaseX, BaseY
drawMinoBlocks:
 push bc
 ld b,(hl) ;blockXOffset (tiles)
 inc hl
 ld d,(hl) ;blockYOffset (tiles)
 inc hl
 push hl
; ld a,(ix+iDataW)
; cp b ; a - b < 0 --> c set, b > a (out of bounds in x)
; jr c,dMBOutOfBounds
 ld e,c
 call multiplyBCSigned
 ld hl,(minoBlockBaseX)
 add hl,bc ;fieldX + minoX + blockX
 ld (minoBlockObj+iDataXL),hl ;overwrites y but it doesn't matter as we set it next
 
; ld a,(ix+iDataH)
; cp d ; a - d < 0 --> c set, d > a (out of bounds in y)
; jr c,dmbOutOfBounds
 ld c,e
 ld b,d
 call multiplyBCSigned
 ld hl,(minoBlockBaseY) ;TODO: make minoBlockBaseY inline
 add hl,bc
 ld a,l ;only valid y <256
 ld (minoBlockObj+iDataY),a
 
 bit 7,h ;check sign
 jp nz, dMBOutOfBounds ;no negative coordinates
 
 push ix
 ld ix,minoBlockObj
 call drawObjectNoReset
 pop ix 
 
dMBOutOfBounds:
 pop hl
 pop bc
 djnz drawMinoBlocks
 ret

;input:
;b,c = multiply, b is signed
;destroy:
;a,hl if negative
multiplyBCSigned:
 bit 7,b
 jr z,positiveBC
negativeBC:
 ld a,b
 neg
 ld b,a
 mlt bc ;positive b*c
 or a,a
 sbc hl,hl
 sbc hl,bc ; -bc
 push hl
 pop bc
 ret
positiveBC:
 mlt bc
 ret

;requires ix as obj data ptr
;de as setDataFromPTR ptr to curdata etc
;this function remains solely for the name joke
nullPTRMino:
 ld h,1<<drawMinoErase
 call drawMinoObject
 ret
 
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
 ld de,dataLocation
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

delayCurve:
; level, ARE, Lock, DAS
; clear is handled by ~60-level, from ~40 to ~10
; all units in frames aside from level
 .db  1, 30, 30, 15
 .db 21, 30, 30, 15
 .db 26, 25, 25, 12
 .db 31, 20, 20, 10
 .db 36, 15, 15, 10
 .db 41, 10, 15, 8
 .db 51, 10, 15, 8
 .db 61, 10, 10, 8
 .db 71,  5, 10, 6
 .db 81,  5,  8, 6
delayCurveEnd:
 .db 255, 0,  0, 0

;format: x ofs, y ofs, spriteID, palette
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

; finally, a REAL rotation 
kicksO:
 .db  0, 0
 .db  0, 1
 .db -1, 1
 .db -1, 0
 .db  0, 0

;this is needed to give important jumps 
;fixed addresses to call from the data file, 
;which doesn't have access to the main program's 
;data. That way, menus can be designed separately
;from the program and allow for more customization.
;this will be copied to PSS+2048
menuJumps:
 jp mainMenu				;return to main menu
 jp initGame				;start game
 jp exit				;exit program
 jp activeMenu				;"runs" a menu object
 jp getStringInList			;gets [a] in menu/list
 jp getStringPTRSelection		;gets selected item
 jp setNumber				;select 8bit number
 jp drawObject				;draw given object
 jp checkKey				;keypress to [a]
 jp swapVRamPTR				;exactly what it says
 jp drawMinoFromType			;draw a mino at a location
menuJumpsEnd:
