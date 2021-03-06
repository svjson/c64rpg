*=$0801
.byte $0c, $08, $0a, $00, $9e, $20
.byte $32, $30, $36, $34, $00, $00
.byte $00

;; +----------------------------------+
;; |                                  |
;; |    INIT ROUTINE                  |
;; |                                  |
;; +----------------------------------+
*=$810

     jsr clearscreen

     lda #$00           ; Set border color
     sta $d020
     lda #$05           ; Set screen background color
     sta $d021
     sta sceneColBg

     lda #$09           ; Set character set color
     sta $d022
     lda #$1d
     sta $d023

     lda $d018          ; Remap character set
     ora #%00001110
     sta $d018

     lda #$18           ; Char multicolour mode on 
     sta $d016

     lda #$00
     sta $fb            ; raster counter

     jsr enterMapMode

     lda #%00000011     ; Enable player sprites
     sta $d015

     jsr initStatusArea

     lda #%01111111     ; Disable CIA interrupts
     sta $dC0d

     and $d011          ; Clear highest bit in raster register
     sta $d011

     lda #$d3           ; Set raster line for irq
     sta $d012

     lda #<enterstatusirq    ; Interrupt vector
     sta $0314
     lda #>enterstatusirq
     sta $0315

     lda #%00000001     ; Enable raster interrupt signals
     sta $d01a

     lda #<dungeoncellar
     sta $20
     lda #>dungeoncellar
     sta $21
     jsr enterArea
     jmp mainloop

enterMapMode:
     lda #$81          ; Set player sprite pointers
     sta $07f8
     lda #$80          ; Set player sprite pointers
     sta $07f9

     lda #$10           ; Set up sprite colors
     sta $d027

     lda #$08
     sta $d028

     lda #$0e
     sta $d025

     lda #$0f
     sta $d026

     lda #$a4           ; Set player sprite coords
     sta $d000
     sta $d002

     lda #$7d
     sta $d001
     sta $d003

     lda #$02           ; Set sprite 2 to multicolor
     sta $d01c

     lda $d018          ; Remap tileset
     and #%11110001
     ora tilesetMask
     sta $d018
     lda sceneCol1      ; Set character set color
     sta $d022
     lda sceneCol2
     sta $d023

     rts

text_HPDUMMY     .text "HP:  012/014"
text_EXPDUMMY    .text "EXP: 050/100"

enterstatusirq   nop
                 nop
                 nop
                 nop
                 nop
                 nop
                 nop
                 lda #$00
                 sta $d021
                 lda $d018              ; Remap tileset
                 ora #%00001110
                 sta $d018

                 lda #%00001000 ; Disable multicolor text mode
                 sta $d016

                 lda #<leavestatusirq    ; Interrupt vector
                 sta $0314
                 lda #>leavestatusirq
                 sta $0315

                 lda #$ff           ; Set raster line for irq
                 sta $d012

                 asl $d019

                 inc $fb      ; wait for anim/delay counter to loop
                 lda #$20
                 cmp $fb
                 bne exitenterirq

                 lda #$00     ; reset anim/delay counter
                 sta $fb

                 jsr animatechars

exitenterirq     jmp $ea31

leavestatusirq   lda sceneColBg
                 sta $d021

                 lda #%00011000 ; Enable multicolor text mode
                 sta $d016
                 lda $d018          ; Remap tileset
                 and #%11110001
                 ora tilesetMask
                 sta $d018

                 lda #<enterstatusirq    ; Interrupt vector
                 sta $0314
                 lda #>enterstatusirq
                 sta $0315

                 lda #$d2          ; Set raster line for next irq
                 sta $d012

                 asl $d019
                 jmp $ea81

;; +----------------------------------+
;; |                                  |
;; |    MAIN GAME LOOP                |
;; |                                  |
;; +----------------------------------+
*=$5000
mainloop
                    jsr readKey

mlcont              lda #$15            ; wait for raster retrace
                    cmp $d012
                    bne mlcont

                    lda screenDirty
                    cmp #$00
                    beq mainloop

                    lda #$00
                    sta $c6             ; Clear keyboard buffer
                    jsr updateSprites
                    jsr drawlevel

                    lda #$00
                    sta screenDirty

                    jmp mainloop

;; +----------------------------------+
;; |                                  |
;; |    KEY INPUT                     |
;; |                                  |
;; +----------------------------------+

key_UP    = #$17            ;; W
key_LEFT  = #$01            ;; A
key_RIGHT = #$04            ;; D
key_DOWN   = #$18           ;; X

key_UPLEFT = #$11           ;; Q
key_UPRIGHT = #$05          ;; E
key_DOWNLEFT = #$1a         ;; Z
key_DOWNRIGHT = #$03        ;; C

key_PICKUP = #$07           ;; G
key_INVENTORY = #$09        ;; I
key_ACTIVATE_TRIGGER = #$14 ;; T

readKey
                    jsr $ffe4
                    and #$3f

                    ldx playerX         ; Load player coordinates into X and Y register to prepare for move
                    ldy playerY

                    cmp key_UP
                    beq move_up

                    cmp key_LEFT
                    beq move_left

                    cmp key_DOWN
                    beq move_down

                    cmp key_RIGHT
                    beq move_right

                    cmp key_UPLEFT
                    beq move_upleft

                    cmp key_UPRIGHT
                    beq move_upright

                    cmp key_DOWNLEFT
                    beq move_downleft

                    cmp key_DOWNRIGHT
                    beq move_downright

                    cmp key_PICKUP
                    beq move_pickup

                    cmp key_INVENTORY
                    beq move_inventory

                    cmp key_ACTIVATE_TRIGGER
                    beq move_activateTrigger

                    cmp #$06
                    beq toggleFOV

                    cmp #$21
                    beq toggleDebugOrMessages

                    jmp endReadKey

move_up             dey
                    jmp attemptMove

move_left           dex
                    jmp attemptMove

move_down           iny
                    jmp attemptMove

move_right          inx
                    jmp attemptMove

move_upleft         dey
                    dex
                    jmp attemptMove

move_upright        dey
                    inx
                    jmp attemptMove

move_downleft       iny
                    dex
                    jmp attemptMove

move_downright      inx
                    iny
                    jmp attemptMove

move_pickup         jmp attemptPickUp
move_inventory      jmp enterInventory
move_activateTrigger jmp attemptActivateTrigger

toggleFOV           lda areaMode
                    eor %10000000
                    sta areaMode
dummyMove           ldx playerX
                    ldy playerY
                    jmp attemptMove

toggleDebugOrMessages
                    lda debugMode
                    ldx #$01
                    cmp #$00
                    beq setDebugMode
                    ldx #$00
setDebugMode        stx debugMode
                    jsr clearMessageRoll
                    jsr refreshMessageArea
                    rts
endReadKey
                    rts

currentTileProps    .byte %00000000


;; +----------------------------------+
;; |                                  |
;; |    MOVEMENT ROUTINES             |
;; |                                  |
;; +----------------------------------+

attemptMove:        ; In parameters: target x, y in X and Y registers
                    stx tmpX                ; Preserve X
                    sty tmpY                ; Preserve Y
                    jsr getTileAt           ; Look up tile at x, y
                    tax                     ; check if passable by comparing with tile table
                    lda tileProps, x
                    sta currentTileProps    ; Keep icon properties for later use
                    and #%10000000
                    cmp #%10000000
                    bne endAttemptMove

                    jsr getNPCAt            ; Check for NPCs at target position
                    cmp #$ff
                    bne interactNPC

                    jmp performMove
endAttemptMove      rts

interactNPC         jmp attackNPC

performMove         ldx tmpX
                    ldy tmpY
                    stx playerX
                    sty playerY

movePerformed       inc screenDirty
                    lda #$0a
                    sta playerTurnCost

                    jsr npcMoves
                    jsr prepareScreenBuffer
                    clc
                    lda areaMode
                    and #%10000000
                    beq triggerCheck
                    jsr updateFOVLines

triggerCheck        lda currentTileProps        ; Check if tile has trigger flag set
                    and #%00100000
                    cmp #%00100000
                    beq evalWalkOnTriggers
moveDone            rts

evalWalkOnTriggers
                    lda var_triggerAction_WALK_ON
checkTriggerOfA     sta trAction
                    jsr evaluateTriggerAt
                    cmp #$00
                    beq moveDone
                    ; Else it is $01, since we only have exittriggers so far
                    jmp enterArea

attemptActivateTrigger:
                    lda var_triggerAction_MAN_ACTIVATE
                    jmp checkTriggerOfA

.include "triggers.asm"
.include "items.asm"
.include "inventory.asm"

;; +----------------------------------+
;; |                                  |
;; |    NPC/MONSTER ROUTINES          |
;; |                                  |
;; +----------------------------------+

iterX .byte $00
iterY .byte $00

attempts .byte $00

testX .byte $00
testY .byte $00

dirX    .byte $00, $01, $01, $01, $00, $ff, $ff, $ff
dirY    .byte $ff, $ff, $00, $01, $01, $01, $00, $ff
dirXMod .byte $00
dirYMod .byte $00
dirMod  .byte $00

npcAP   .byte $00
npcCost .byte $00

getNPCAt:
                    ldx #$00
                    cpx npcTableSize
                    beq endGetNPCAt
                    jsr prepareNPCIter
getNPCAtLoop        lda ($20), y
                    and #%10000000
                    cmp #%10000000
                    bne getNPCNextIter
                    iny
                    lda ($20), y
                    cmp tmpX
                    bne getNPCNextIter
                    iny
                    lda ($20), y
                    cmp tmpY
                    bne getNPCNextIter
                    txa
                    rts
getNPCNextIter      inx
                    cpx npcTableSize
                    beq endGetNPCAt
                    ldy #$00
                    jsr inc20Ptr
                    jmp getNPCAtLoop
endGetNPCAt         lda #$ff            ; Return $ff(= no match) in A
                    rts

prepareNPCIter:
                    lda #>npcTable
                    sta $21
                    lda #<npcTable
                    sta $20
                    lda npcTableRowSize
                    sta inc20ModVal
                    ldy #$00
                    rts

npcMoves:
                    ldx #$00
                    cpx npcTableSize
                    beq endNPCMoves
                    jsr prepareNPCIter
                    stx iterX
                    stx attempts
npcTurnBegin
                    lda ($20), y
                    and #%10000000
                    cmp #%10000000
                    bne npcMoveNextIter

                    clc
                    ldy var_npcCurrentAP        ; Add cost of players last move to current NPCs AP
                    lda ($20), y
                    adc playerTurnCost
                    sta ($20), y
                    sta npcAP

                    ldy var_npcMoveCost         ; Check if current AP is enough for move
                    lda ($20), y
                    sta npcCost
                    cmp npcAP
                    bcs npcMoveNextIter

                    clc                         ; Deduct NPCs cost of move from AP
                    lda npcAP
                    sbc npcCost
                    ldy var_npcCurrentAP
                    sta ($20), y

                    ldy var_npcXPos
npcMoveLoop
                    ldx iterX

                    lda ($20), y               ; Load up NPC position into tmpX, tmpY
                    sta tmpX
                    iny
                    lda ($20), y
                    sta tmpY

                    ldy var_npcMode            ; Check NPC current mode
                    lda ($20), y
                    cmp #$01
                    beq npcMoveRandomJmp
                    cmp #$02
                    beq npcFollowPlayer
                    jmp npcMoveNextIter

npcMoveRandomJmp    jmp npcMoveRandom

npcMoveNextIter
                    inc iterX
                    ldx iterX
                    cpx npcTableSize
                    beq endNPCMoves
                    lda npcTableRowSize
                    sta inc20ModVal
                    jsr inc20Ptr
                    ldy #$00
                    sty attempts
                    jmp npcTurnBegin

endNPCMoves         rts

npcFollowPlayer:
                    lda #$00
                    sta dirXMod
                    sta dirYMod
                    sta iter
                    lda tmpX
                    sta testX
                    lda tmpY
                    sta testY
npcFollowCheckHor
                    lda playerX
                    cmp tmpX
                    beq npcFollowCheckVert
                    bcc npcFollowLeft
npcFollowRight      lda #$01
                    sta dirXMod
                    jmp npcFollowCheckVert
npcFollowLeft       lda #$ff
                    sta dirXMod

npcFollowCheckVert  lda playerY
                    cmp tmpY
                    beq npcFollowTryMove
                    bcc npcFollowUp

npcFollowDown       lda #$01
                    sta dirYMod
                    jmp npcFollowTryMove

npcFollowUp         lda #$ff
                    sta dirYMod

npcFollowTryMove    ldx #$00
npcFollowFindDirMod lda dirXMod
                    cmp dirX, x
                    bne npcFollowFindDirNxt
                    lda dirYMod
                    cmp dirY, x
                    bne npcFollowFindDirNxt
                    jmp npcFollowTryDir
npcFollowFindDirNxt inx
                    jmp npcFollowFindDirMod
npcFollowTryDir
                    txa
                    sta dirMod
                    jsr applyDirMod

                    lda tmpY
                    cmp playerY
                    bne npcFollowTryMove2

                    lda tmpX
                    cmp playerX
                    bne npcFollowTryMove2
                    jmp npcAttackPlayer

npcFollowTryMove2   jsr getTileAt
                    tay
                    lda tileProps, y
                    and #%10000000
                    cmp #%10000000
                    bne npcFollowNextAttempt
                    jmp npcPerformMove

npcFollowNextAttempt inc iter
                     lda #$03
                     cmp iter
                     beq jmpNpcMoveNextIter
                     lda #$01
                     cmp iter
                     bne npcFollowLastAttempt
                     dec dirMod
                     jmp npcFollowRetry
npcFollowLastAttempt inc dirMod
                     inc dirMod
npcFollowRetry       lda testX
                     sta tmpX
                     lda testY
                     sta tmpY
                     jsr wrapDirMod
                     ldx dirMod
                     jmp npcFollowTryDir

wrapDirMod           lda dirMod
                     cmp #$08
                     beq highWrap
                     cmp #$ff
                     beq lowWrap
                     rts
highWrap             lda #$00
                     sta dirMod
                     rts
lowWrap              lda #$07
                     sta dirMod
                     rts

jmpNpcMoveNextIter  jmp npcMoveNextIter

applyDirMod         tax                 ; Apply dir to tmpX/tmpY
                    lda tmpX
                    clc
                    adc dirX, x
                    sta tmpX
                    clc
                    lda tmpY
                    adc dirY, x
                    sta tmpY
                    rts

retryNPCMove        lda #$05
                    cmp attempts
                    beq jmpNpcMoveNextIter
                    ;inc attempts
                    ldy var_npcXPos
                    jmp npcMoveLoop

npcMoveRandom:      jsr rndNum          ; Select a random dir
                    sta num1
                    lda #32
                    sta num2
                    lda #$07
                    sta num3
                    jsr divide_rndup

                    jsr applyDirMod

                    ldx tmpX            ; Evaluate target position
                    ldy tmpY
                    jsr getTileAt
                    tay
                    lda tileProps, y
                    and #%10000000
                    cmp #%10000000
                    bne retryNPCMove

                    lda playerX
                    cmp tmpX
                    bne npcPerformMove

                    lda playerY
                    cmp tmpY
                    beq retryNPCMove

npcPerformMove      lda tmpX
                    ldy #$01
                    sta ($20), y
                    lda tmpY
                    iny
                    sta ($20), y
                    jmp npcMoveNextIter

npcAttackPlayer:
                    lda $20             ; Save NPC pointer
                    sta tmpPtr2
                    lda $21
                    sta tmpPtr2+1

                    ldy #$05            ; Store name pointer hi byte in tmpPtr1
                    lda ($20), y
                    sta tmpPtr1

                    iny                 ; Store name pointer lo byte in tmpPtr1
                    lda ($20), y
                    sta tmpPtr1+1

                    lda #<text_THE
                    sta $20
                    lda #>text_THE
                    sta $21
                    jsr addToMessageBuffer

                    lda tmpPtr1
                    sta $20
                    lda tmpPtr1+1
                    sta $21
                    jsr addToMessageBuffer

                    lda #$20
                    jsr addCharToMessageBuffer

                    lda #<text_HITS_YOU
                    sta $20
                    lda #>text_HITS_YOU
                    sta $21
                    jsr addToMessageBuffer
                    jsr addMessage

                    lda tmpPtr2
                    sta $20
                    lda tmpPtr2+1
                    sta $21

                    jmp npcMoveNextIter


damageInflicted   .byte $00

; $20-21 - Pointer to NPC being attacked
attackNPC:
                    ldy var_npcMode     ; NPC is being attacked, so now hostile
                    lda #$02
                    sta ($20), y

                    jsr rndNum          ; Simple calc, arbitrary random damage (1-6)+2
                    sta num1
                    lda #64
                    sta num2
                    lda #04
                    sta num3
                    jsr divide_rndup

                    clc
                    adc #$02
                    sta damageInflicted

                    lda $20             ; Save NPC pointer
                    sta tmpPtr2
                    lda $21
                    sta tmpPtr2+1

                    ldy #$05            ; Store name pointer hi byte in tmpPtr1
                    lda ($20), y
                    sta tmpPtr1

                    iny                 ; Store name pointer lo byte in tmpPtr1
                    lda ($20), y
                    sta tmpPtr1+1

                    lda #<text_YOU_HIT_THE
                    sta $20
                    lda #>text_YOU_HIT_THE
                    sta $21
                    jsr addToMessageBuffer

                    lda tmpPtr1
                    sta $20
                    lda tmpPtr1+1
                    sta $21
                    jsr addToMessageBuffer

                    lda #$20
                    jsr addCharToMessageBuffer

                    lda #<text_FOR
                    sta $20
                    lda #>text_FOR
                    sta $21
                    jsr addToMessageBuffer

                    ldx damageInflicted
                    jsr byte_to_decimal
                    jsr addToMessageBuffer

                    lda #$20
                    jsr addCharToMessageBuffer

                    lda #<text_HP
                    sta $20
                    lda #>text_HP
                    sta $21
                    jsr addToMessageBuffer
                    jsr addMessage

                    lda tmpPtr2
                    sta $20
                    lda tmpPtr2+1
                    sta $21

                    clc
                    ldy #$07
                    lda damageInflicted
                    cmp ($20), y
                    bcs npcKilled

                    lda ($20), y
                    sbc damageInflicted
                    sta ($20), y
                    jmp movePerformed

npcKilled:
                    lda #%00000000      ; Set NPC to OFF
                    ldy #$00
                    sta ($20), y

                    ldy #$05            ; Store name pointer hi byte in tmpPtr1
                    lda ($20), y
                    sta tmpPtr1

                    iny                 ; Store name pointer lo byte in tmpPtr1
                    lda ($20), y
                    sta tmpPtr1+1

                    lda #<text_YOU_KILLED_THE   ; Add message to buffer
                    sta $20
                    lda #>text_YOU_KILLED_THE
                    sta $21
                    jsr addToMessageBuffer

                    lda tmpPtr1
                    sta $20
                    lda tmpPtr1+1
                    sta $21
                    jsr addToMessageBuffer

                    jsr addMessage
                    jmp movePerformed

;; +----------------------------------+
;; |                                  |
;; |    AREA LOADING ROUTINES         |
;; |                                  |
;; +----------------------------------+
.include "areaload.asm"

;; +----------------------------------+
;; |                                  |
;; |    SCREEN BUFFER ROUTINES        |
;; |                                  |
;; +----------------------------------+
tilesetMask .byte %00000000
sceneColors:
sceneColBg  .byte $00
sceneCol1   .byte $00
sceneCol2   .byte $00

prepareScreenBuffer:
                   lda #>screenBuffer ; Screen Buffer offset
                   sta $21
                   lda #<screenBuffer
                   sta $20

                   lda #$14
                   sta inc20ModVal

                   lda #>currentArea ; Level area offset in memory
                   sta $23
                   lda #<currentArea
                   sta $22

                   lda currentAreaHeight
                   sta drawBufferHeight
                   lda currentAreaWidth
                   sta drawBufferWidth

                   clc
                   lda playerY                    ; set up area offset relative to player coordinates
                   sbc #$04
                   sta currentAreaOffsetY

                   clc
                   lda playerX
                   sbc #$08
                   sta currentAreaOffsetX

                   lda currentAreaOffsetY         ; Set up counter for area row to be copied
                   sta areaRow

                   ldx #$00                       ; Now offset pointer to the area data according to the screenOffsetY. 
psbOffsetLoop      cpx currentAreaOffsetY         ; Move level area offset pointers in $22, $23 according currentAreaOffsetY
                   beq psbOffsetCalculated         ; Nothing to do if Y is 0

                   lda currentAreaOffsetY         ; Now inc or dec offset counter until it matches the Y offset
                   cmp #$80
                   bcc psbOffsetPositive

psbOffsetNegative   dex
                    lda $22
                    sbc currentAreaWidth
                    sta $22
                    bcs psbOffsetLoop
                    dec $23
                    jmp psbOffsetLoop

psbOffsetPositive   inx
                    lda $22
                    adc currentAreaWidth
                    sta $22
                    bcc psbOffsetLoop
                    inc $23
                    jmp psbOffsetLoop

psbOffsetCalculated  lda #$00                   ; Set counters to zero
                     sta crsr
                     sta iter
                     sta drawat

                     clc
                     lda #$14                   ; Set lineEnd to current area X offset + screen width in tiles
                     adc currentAreaOffsetX     ; to indicate that we are done with a line when crsr has reached
                     sta lineEnd                ; that X position of level data

copyToScreenBufferLoop
                     inc iter
                     ldx iter

                     jsr copyLineToScreenBuffer
                     jsr incleveloffset
                     jsr inc20Ptr
                     inc areaRow

                     lda #$00
                     sta drawat
                     sta crsr

                     ldx iter
                     cpx #$0a
                     bne copyToScreenBufferLoop
                     jmp applyItems
                     rts

copyLineToScreenBuffer
                     lda currentAreaOffsetX         ; Set up counter for area col to be drawn
                     sta areaCol

copyLineLoop
                     lda areaRow
                     cmp drawBufferHeight
                     bcs psbEmptytile
                     lda areaCol
                     cmp drawBufferWidth
                     bcs psbEmptytile
                     jmp psbTile

psbEmptytile         lda #$00
                     jmp psbOutTile

psbTile              ldy areaCol       ; Load source block index
                     lda ($22), y

psbOutTile           ldy crsr
                     sta ($20), y
                     inc crsr
                     inc areaCol
                     ldy areaCol
                     cpy lineEnd
                     bne copyLineLoop
                     rts

applyItems:
                     lda #>itemTable
                     sta $25
                     lda #<itemTable
                     sta $24

                     ldx #$00
                     cpx itemTableSize
                     beq itemPrepEnd
itemPosLoop          ldy #$00
                     lda ($24), y          ; Load item status flag
                     and #%10000000
                     cmp #%10000000
                     bne noItem
                     iny

                     clc
                     lda ($24), y
                     adc #$01
                     sbc currentAreaOffsetX
                     cmp #$00
                     bmi noItem
                     clc
                     cmp #$14
                     bpl noItem
                     sta npcOffset
                     sta tmpX
                     iny

                     clc
                     lda ($24), y
                     adc #$01
                     sbc currentAreaOffsetY
                     cmp #$00
                     bmi noItem
                     clc
                     cmp #$0a
                     bpl noItem
                     sta tmpY
                     tay
                     lda powersOf20, y
                     adc npcOffset
                     sta npcOffset

                     ldy #$03
                     lda ($24), y
                     ldy npcOffset
                     sta screenBuffer, y

                     jmp itemChecked
noItem:
itemChecked:
                     inx
                     cpx itemTableSize
                     bne prepNextItemLoop
itemPrepEnd          jmp applyNpcs
prepNextItemLoop
                     clc
                     lda $24
                     adc itemTableRowSize
                     sta $24
                     bcc nextItemNoCarry
                     inc $25
nextItemNoCarry      jmp itemPosLoop

applyNpcs:
                     lda #>npcTable
                     sta $25
                     lda #<npcTable
                     sta $24

                     ldx #$00
                     stx noofActiveSprites
                     cpx npcTableSize
                     beq npcPrepEnd
npcPosLoop           ldy #$00
                     lda ($24), y          ; Load npc status flag
                     and #%10000000
                     cmp #%10000000
                     bne noNpc
                     iny

                     clc
                     lda ($24), y
                     adc #$01
                     sbc currentAreaOffsetX
                     cmp #$00
                     bmi noNpc
                     clc
                     cmp #$14
                     bpl noNpc
                     sta npcOffset
                     sta tmpX
                     iny

                     clc
                     lda ($24), y
                     adc #$01
                     sbc currentAreaOffsetY
                     cmp #$00
                     bmi noNpc
                     clc
                     cmp #$0a
                     bpl noNpc
                     sta tmpY
                     tay
                     lda powersOf20, y
                     adc npcOffset
                     sta npcOffset

                     ldy #$03
                     lda ($24), y
                     ldy npcOffset
                     sta screenBuffer, y
                     tya
                     ldy noofActiveSprites
                     sta activeSpriteTileOffsets, y

                     ldy #$04           ; Set sprite pointer
                     lda ($24), y
                     ldy noofActiveSprites
                     sta activeSpritePtrs, y

                     lda tmpX
                     sta activeSpriteOffsetX, y
                     lda tmpY
                     sta activeSpriteOffsetY, y

                     inc noofActiveSprites
                     jmp npcChecked
noNpc:
npcChecked:
                     inx
                     cpx npcTableSize
                     bne prepNextNpcLoop
npcPrepEnd           rts
prepNextNpcLoop
                     clc
                     lda $24
                     adc npcTableRowSize
                     sta $24
                     bcc nextNpcNoCarry
                     inc $25
nextNpcNoCarry
                     jmp npcPosLoop

npcOffset .byte $00

;; +----------------------------------+
;; |                                  |
;; |    FOV ROUTINES                  |
;; |                                  |
;; +----------------------------------+

; Clear FOV Buffer between runs
clearFOVBuffer:
                    lda #$10
                    ldx #$00
                    lda #$00
clearFOVLoop        sta fovBuffer, x
                    inx
                    cpx #$c8
                    bne clearFOVLoop
                    rts

; FOV Routine local variables
segmentOffsets .byte $00, $00, $00, $00, $00
currentLine .byte $00               ; Track current line of FOV sector being investigated
currentLineSegment .byte $00        ; Track the current segment of the line being investigated
lineLength  .byte $00               ; Length of the current line
fovSectorDir .byte $00              ; N, S, W, E

; FOV Routine constants
segment1Start   .byte 88,   130,    128,    90
segment2Start   .byte 67,   151,    147,    71
segment3Start   .byte 46,   172,    166,    52
segment4Start   .byte 25,   193,    185,    33
segment5Start   .byte 07,   211,    144,    72
dirMods .byte 01, 255, 236, 20
segMasks  .byte %10000000
          .byte %01000000
          .byte %00100000
          .byte %00010000
          .byte %00001000
fovLineTable .byte %00000011  ; A-1     - 1
             .byte %00110100  ; A-2     - 2
             .byte %01010100  ; A-3     - 3
             .byte %10101100  ; B-1     - 4
             .byte %01010101  ; B-2     - 5
             .byte %00100101  ; B-3     - 6
             .byte %00011101  ; B-4     - 7
             .byte %00011101  ; B-5     - 8
             .byte %00100101  ; B-6     - 9
             .byte %01010100  ; B-7     - 10
             .byte %10100100  ; C-1     - 11
             .byte %01010100  ; C-1     - 12

; Walk FOV lines, sector by sector
updateFOVLines:
                    jsr clearFOVBuffer

revealPlayerTile    lda screenBuffer+109            ; Reveal tile player is standing on
                    sta fovBuffer+109
                    lda #$00
                    sta fovSectorDir
walkFOVSector:
                    ldx fovSectorDir
                    lda segment1Start, x
                    sta segmentOffsets
                    lda segment2Start, x
                    sta segmentOffsets+1
                    lda segment3Start, x
                    sta segmentOffsets+2
                    lda segment4Start, x
                    sta segmentOffsets+3
                    lda segment5Start, x
                    sta segmentOffsets+4
                    ldx #$00                        ; Set line index to 0 (of c)
                    stx currentLine

walkFOVLine:        ;; Assume currentLine is loaded into X when we return here
                    lda fovLineTable, x             ; Store away line length
                    and #%00000111
                    sta lineLength
                    ldx #$00                        ; Set line segment look counter to zero
                    stx currentLineSegment
walkSegmentsLoop    ldy segmentOffsets, x           ; Copy tile to target fovBuffer
                    lda screenBuffer, y
                    sta fovBuffer, y

                    tay                             ; End line if the discovered tile blocks sight
                    lda tileProps, y
                    and #%01000000
                    cmp #%01000000
                    bne lineWalked

                    inx                             ; Continue looping if we haven't reached the last line segment
                    cpx lineLength
                    bne walkSegmentsLoop

lineWalked          inc currentLine
                    ldy currentLine
                    cpy #$0c                        ; 0c for all lines, There are twelve lines to a sector
                    bne prepareNextLine

                    inc fovSectorDir
                    lda fovSectorDir
                    cmp #$04
                    beq endOfFov
                    jmp walkFOVSector
endOfFov
                    rts

prepareNextLine
                    ldx fovSectorDir                ; Set up segments modifiers for next round
                    lda dirMods, x
                    sta modVal
                    ldy currentLine
                    ldx #$00

modSegmentLoop      clc                             ; Check if fovTable dictates seg mod
                    lda fovLineTable, y
                    and segMasks, x
                    cmp segMasks, x
                    bne noSegMod

modSegmentOffset    lda segmentOffsets, x           ; Apply segmod to segments
                    clc
                    adc modVal
                    sta segmentOffsets, x
noSegMod
                    inx                             ; Walk next line
                    cpx #$05
                    bne modSegmentLoop
                    ldx currentLine
                    jmp walkFOVLine

;; +----------------------------------+
;; |                                  |
;; |    SPRITE POSITIONING ROUTINES   |
;; |                                  |
;; +----------------------------------+
noofActiveSprites .byte $00

activeSpriteTileOffsets .byte $00, $00, $00, $00, $00, $00
activeSpritePtrs        .byte $00, $00, $00, $00, $00, $00
activeSpriteOffsetX     .byte $00, $00, $00, $00, $00, $00
activeSpriteOffsetY     .byte $00, $00, $00, $00, $00, $00

sprite9thBitMask .byte %00000000
spritePrepMask  .byte %00000011
npcSpriteMasks  .byte %00000100
                .byte %00001000
                .byte %00010000
                .byte %00100000
                .byte %01000000
                .byte %10000000


updateSprites:
                   lda noofActiveSprites
                   lda #%00000000
                   sta sprite9thBitMask
                   lda #%00000011
                   sta spritePrepMask
                   ldx #$00
                   cpx noofActiveSprites
                   beq spritesUpdated

spriteUpdateLoop:
                   lda activeSpritePtrs, x      ; Set sprite pointer
                   sta $07fa, x

                   ldy activeSpriteOffsetX, x
                   cpy #$0f
                   bcc setSpriteCoords
                   lda npcSpriteMasks, x
                   ora sprite9thBitMask
                   sta sprite9thBitMask

setSpriteCoords    clc
                   lda powersOf16, y
                   sta tmpX
                   ldy activeSpriteOffsetY, x
                   lda powersOf16, y
                   sta tmpY

                   lda tmpX
                   adc #$18                     ; Add border padding
                   ldy powersOf2, x
                   sta $d004, y
                   clc
                   lda tmpY
                   adc #$32                     ; Add border padding
                   ldy powersOf2, x
                   iny
                   sta $d004, y

                   lda npcSpriteMasks, x        ; Activate sprite
                   ora spritePrepMask
                   sta spritePrepMask
                   lda #$00
                   sta $d029, x

                   inx
                   cpx noofActiveSprites
                   bne spriteUpdateLoop
spritesUpdated:
                   rts

npcSpritesTempRoutine
                     ldy #$04           ; Set sprite pointer
                     lda ($24), y
                     lda #$89
                     sta $07fa, x

                     ldy tmpX
                     lda powersOf16, y
                     sta tmpX
                     ldy tmpY
                     lda powersOf16, y
                     sta tmpY

                     lda tmpX
                     adc #$18
                     ldy powersOf2, x
                     sta $d004, y
                     lda tmpY
                     adc #$32
                     ldy powersOf2, x
                     iny
                     sta $d004, y

                     lda npcSpriteMasks, x
                     ora spritePrepMask
                     sta spritePrepMask
                     lda #$00
                     sta $d029, x

                     rts

;; +----------------------------------+
;; |                                  |
;; |    LEVEL DRAWING ROUTINES        |
;; |                                  |
;; +----------------------------------+

drawlevel;
                   lda #$04 ; Screen offset
                   sta $21
                   lda #$00
                   sta $20

                   lda #$d8 ; Color RAM offset
                   sta $25
                   lda #$00
                   sta $24

                   lda #$14
                   sta inc22ModVal

                   lda areaMode             ; Jump to FOV mode setup of offsets and level data if FOV is on
                   and #%10000000
                   bne prepareFOVMode

                   lda #>screenBuffer        ; Screen buffer offset in memory
                   sta $23
                   lda #<screenBuffer
                   sta $22
                   jmp dlOffsetCalculated

prepareFOVMode
                   lda #>fovBuffer ; Level area offset in memory
                   sta $23
                   lda #<fovBuffer
                   sta $22

dlOffsetCalculated  lda #$00                   ; Set counters to zero
                    sta crsr
                    sta iter
                    sta drawat

drawlevelloop
                    inc iter
;                    inc $d020

                    jsr drawline
                    jsr incscreenoffset

                    lda #$0a
                    cmp iter
                    bne drawlevelloop

                    lda sprite9thBitMask
                    sta $d010
                    lda spritePrepMask
                    sta $d015

                    lda #$00
                    sta $d020

                    lda debugMode
                    cmp #$01
                    bne endDrawLevel

                    jsr refreshMessageArea

endDrawLevel        rts

incscreenoffset
                    lda $20
                    clc
                    adc #$50
                    bcc screennocarry
                    inc $21
                    inc $25
screennocarry
                    sta $20
                    sta $24
                    rts

incleveloffset
                    lda $22
                    clc
                    adc drawBufferWidth
                    bcc levelnocarry
                    inc $23
levelnocarry
                    sta $22
                    rts

drawline
                    ldy #$00
                    sty drawat
                    clc

drawlineloop
                    ldy crsr
                    lda ($22), y
                    tax
drawtile
                    ldy drawat
                    lda tileChar1, x        ; Draw upper row of tile chars to screen
                    sta ($20), y
                    lda tileCharColor1, x
                    sta ($24), y

                    iny
                    lda tileChar2, x
                    sta ($20), y
                    lda tileCharColor2, x
                    sta ($24), y

                    tya                     ; Forward to lower row
                    adc #$27
                    tay

                    lda tileChar3, x        ; Draw lower row of tile chars to screen 
                    sta ($20), y
                    lda tileCharColor3, x
                    sta ($24), y

                    iny
                    lda tileChar4, x
                    sta ($20), y
                    lda tileCharColor4, x
                    sta ($24), y

                    ldy drawat
                    iny
                    iny
                    sty drawat

                    inc crsr

                    cpy #$28
                    bne drawlineloop

                    rts

iter
    .byte $00

drawat
    .byte $00

crsr
    .byte $00

areaRow .byte $00
areaCol .byte $00

lineEnd .byte $14

tmpX = $0802
tmpY = $0803

;; +----------------------------------+
;; |                                  |
;; |    AREA FUNCTIONS                |
;; |                                  |
;; +----------------------------------+

; Input: coords in X and Y. Return tile byte as A
getTileAt:
                    ldy #$00
                    lda #>currentArea
                    sta $25
                    lda #<currentArea
                    sta $24

getTileYOffLoop     cpy tmpY
                    beq resolveTile
                    iny
                    lda $24
                    clc
                    adc currentAreaWidth
                    sta $24
                    bcc getTileYOffLoop
                    inc $25
                    jmp getTileYOffLoop

resolveTile
                    ldy tmpX
                    lda ($24), y
                    rts

.include "dungeon_generation.asm"

;; +----------------------------------+
;; |                                  |
;; |    STATUS BAR ROUTINES           |
;; |                                  |
;; +----------------------------------+
*=$0940
messageLineLength = #24

messageRow1 = $0749
messageRow2 = $0771
messageRow3 = $0799
messageRowCol1 = $db49
messageRowCol2 = $db71
messageRowCol3 = $db99

.enc petscii	;define an ascii->petscii encoding
.cdef " @", 32  ;characters
.cdef "AZ", $01
.cdef "az", $01

text_PURSE          .byte 5
                    .text "PURSE"
text_FLOOR          .byte 5
                    .text "FLOOR"
text_BACKPACK       .byte 8
                    .text "BACKPACK"
text_GOLD           .byte 4
                    .text "GOLD"

text_YOU_KILLED_THE .byte 15
                    .text "YOU KILLED THE "
text_YOU_HIT_THE    .byte 12
                    .text "YOU HIT THE "
text_HITS_YOU       .byte 9
                    .text "HITS YOU "
text_FOR            .byte 4
                    .text "FOR "
text_THE            .byte 4
                    .text "THE "
text_HP             .byte 2
                    .text "HP"
text_PICKED_UP      .byte 10
                    .text "PICKED UP "
text_NOTHING_TO_PICK_UP .byte 23
                    .text "NOTHING TO PICK UP HERE"
text_OF             .byte 3
                    .text "OF "
text_NOTHING        .byte 7
                    .text "NOTHING"
text_WC             .byte 3
                    .text "WC:"
text_AC             .byte 3
                    .text "AC:"

messageBufferLength .byte $00
messageBuffer .text "ABC                                                        "
linesWritten .byte $00

addCharToMessageBuffer:
                    ldx messageBufferLength
                    sta messageBuffer, x
                    inc messageBufferLength
                    rts

addToMessageBuffer:
                    clc
                    lda #<messageBuffer
                    sta $22
                    lda #>messageBuffer
                    sta $23
                    lda messageBufferLength
                    sta inc22ModVal
                    jsr inc22Ptr
                    ldy #$00
                    lda ($20), y
                    sta memcpy_rowSize
                    lda #$01
                    sta memcpy_rows
                    inc $20
                    jsr memcpy
                    lda messageBufferLength
                    clc
                    adc memcpy_rowSize
                    sta messageBufferLength
                    rts

addMessage:
                    lda messageBufferLength
                    jsr rollMessages
                    lda #$00
                    sta linesWritten
                    lda #<messageBuffer
                    sta $20
                    sta $24
                    lda #>messageBuffer
                    sta $21
                    sta $25

outputLine          clc
                    ldy messageBufferLength
                    sty memcpy_rowSize
                    cpy messageLineLength
                    bcc outputMessage
                    ldy messageLineLength
findPrevSpaceLoop   lda ($20), y
                    cmp #$20
                    beq spaceFound
                    dey
                    jmp findPrevSpaceLoop
spaceFound
                    sty memcpy_rowSize
                    lda messageLineLength
                    cmp memcpy_rowSize
                    bcc findPrevSpaceLoop
                    ldy #$00
                    lda #$00
blackOutLoop        sta $db99, y
                    iny
                    cpy #$22
                    bne blackOutLoop
outputMessage
                    lda #$01
                    sta memcpy_rows
                    lda #<messageRow3
                    sta $22
                    lda #>messageRow3
                    sta $23
                    jsr memcpy

                    ldy memcpy_rowSize
                    lda #$20
clearRestLoop       cpy messageLineLength
                    beq lineWritten
                    sta messageRow3, y
                    iny
                    jmp clearRestLoop

lineWritten         inc linesWritten
                    clc
                    lda messageBufferLength
                    sbc memcpy_rowSize
                    sta messageBufferLength
                    inc messageBufferLength
                    lda messageBufferLength
                    cmp #$00
                    beq endAddMessage
                    lda memcpy_rowSize
                    sta num1
                    jsr rollMessages
                    lda num1
                    sta memcpy_rowSize
                    sta inc20ModVal
                    inc inc20ModVal             ; Get rid of the space that broke the line
                    dec messageBufferLength      ;
                    lda $24
                    sta $20
                    lda $25
                    sta $21
                    jsr inc20Ptr
                    jmp outputLine

endAddMessage       lda #$00
                    sta messageBufferLength
                    sta rastCtr

                    ldx #$00
fadeInNewMsg        lda #$15     ; wait for raster retrace
                    cmp $d012
                    bne fadeInNewMsg
                    inc rastCtr
                    lda rastCtr
                    cmp #$05
                    bne fadeInNewMsg

                    lda #$00
                    sta fadeLine
                    cpx #08
                    beq msgRts
                    lda #<messageRowCol3
                    sta $20
                    lda #>messageRowCol3
                    sta $21
                    ldy #$00
                    lda fadeColors, x
fadeInLine          sta ($20), y
                    iny
                    cpy messageLineLength
                    bne fadeInLine

                    inc fadeLine
                    ldy fadeLine
                    cpy linesWritten
                    bne prepNextFadeLine
nextIter            inx
                    lda #$00
                    sta fadeLine
                    sta rastCtr
                    jmp fadeInNewMsg

msgRts              rts

prepNextFadeLine
                   lda $20
                   clc
                   sbc #$27
                   sta $20
                   ldy #$00
                   lda fadeColors, x
                   jmp fadeInLine

rastCtr .byte $00
fadeLine .byte $00
fadeColors .byte $00, $0b, $06, $0c, $0e, $0f, $03, $01

rollMessages:
                    lda messageLineLength
                    sta memcpy_rowSize
                    lda #$01
                    sta memcpy_rows
                    lda #<messageRow1
                    sta $22
                    lda #>messageRow1
                    sta $23
                    lda #<messageRow2
                    sta $20
                    lda #>messageRow2
                    sta $21
                    jsr memcpy

                    lda #<messageRowCol1
                    sta $22
                    lda #>messageRowCol1
                    sta $23
                    lda #<messageRowCol2
                    sta $20
                    lda #>messageRowCol2
                    sta $21
                    jsr memcpy

                    lda #<messageRowCol2
                    sta $22
                    lda #>messageRowCol2
                    sta $23
                    lda #<messageRowCol3
                    sta $20
                    lda #>messageRowCol3
                    sta $21
                    jsr memcpy

                    lda #<messageRow2
                    sta $22
                    lda #>messageRow2
                    sta $23
                    lda #<messageRow3
                    sta $20
                    lda #>messageRow3
                    sta $21
                    jsr memcpy

                    rts

clearMessageRoll:
                    lda #>$0749
                    sta $21
                    lda #<$0749
                    sta $20
                    lda #$28
                    sta inc20ModVal

                    ldx #$00
clearMsgAreaRowLoop lda #$20
                    ldy #$00
clearMsgAreaColLoop sta ($20), y
                    iny
                    cpy messageLineLength
                    bne clearMsgAreaColLoop
                    inx
                    cpx #$03
                    beq endClearMessageRoll
                    jsr inc20Ptr
                    jmp clearMsgAreaRowLoop

endClearMessageRoll
                    rts


refreshMessageArea:
                lda debugMode
                cmp #$01
                beq outputPlayerCoordinates
                rts

outputPlayerCoordinates
                    lda #$18               ; Output Player X Coordinate to status area
                    sta $0749
                    lda #<$074b
                    sta print_target
                    lda #>$074b
                    sta print_target+1
                    ldx playerX
                    jsr print_decimal

                    lda #$19               ; Output Player Y Coordinate to status area
                    sta $0771
                    lda #<$0773
                    sta print_target
                    lda #>$0773
                    sta print_target+1
                    ldx playerY
                    jsr print_decimal
                    rts

initStatusArea:
                lda #$27
                sta boxWidth
                lda #$05
                sta boxHeight
                lda #$00
                sta boxLeft
                lda #$14
                sta boxTop
                jsr drawBox

                lda #$0f            ; Print dummy HP/EXP
                sta $db62
                sta $db63

                lda #$0f
                sta $db8a
                sta $db8b
                sta $db8c

                sta $db6a
                sta $db92

                lda #<text_HPDUMMY
                sta print_source
                lda #>text_HPDUMMY
                sta print_source+1

                lda #<$0762
                sta print_target
                lda #>$0762
                sta print_target+1
                lda #$0c
                sta print_source_length
                jsr print_string

                lda #<text_EXPDUMMY
                sta print_source
                lda #>text_EXPDUMMY
                sta print_source+1

                lda #<$078a
                sta print_target
                lda #>$078a
                sta print_target+1
                lda #$0c
                sta print_source_length
                jsr print_string

                rts

boxWidth        .byte $00
boxHeight       .byte $00
boxTop          .byte $00
boxLeft         .byte $00
boxCounter      .byte $00

drawBox:        lda boxTop
                sta targetLine
                jsr forwardToLine

                lda #$00
                ldy boxLeft
                sta ($20), y

                tya
                clc
                adc boxWidth
                sta boxCounter
                iny

drawTopBord     cpy boxCounter
                beq drawBoxCrnr2
                lda #$1b
                sta ($20), y
                iny
                jmp drawTopBord

drawBoxCrnr2    lda #$1c
                sta ($20), y

                inc targetLine
                lda boxHeight
                sta boxCounter
                dec boxCounter
                dec boxCounter

                ldx #$00
                ldy boxLeft
drawBoxVert     jsr inc20Ptr
                cpx boxCounter
                beq drawBoxBottom
                lda #$1d
                sta ($20), y
                tya
                clc
                adc boxWidth
                tay
                lda #$1d
                sta ($20), y
                ldy boxLeft
                inx
                jmp drawBoxVert

drawBoxBottom   lda #$1f
                sta ($20), y

                lda boxLeft
                clc
                adc boxWidth
                sta boxCounter
drawBottBord    iny
                cpy boxCounter
                beq drawBoxEnd
                lda #$1b
                sta ($20), y
                jmp drawBottBord

drawBoxEnd      lda #$1e
                sta ($20), y
                rts

targetLine .byte $00

forwardToLine:
                lda #<$0400
                sta $20
                lda #>$0400
                sta $21
                lda #$28
                sta inc20ModVal
                ldx #$00
forwardLoop     cpx targetLine
                beq forwardedToLine
                jsr inc20Ptr
                inx
                jmp forwardLoop
forwardedToLine rts

;; ----------------------
;; ANIMATE LEVEL CHARS
;; ----------------------
animatechars
    ; anim water
    lda #$3a
    sta $2e

    lda #$31
    sta $2d
    jsr leftshift_2d
    jsr leftshift_2d

    lda #$32
    sta $2d
    jsr leftshift_2d
    jsr leftshift_2d

    lda #$35
    sta $2d
    jsr rightshift_2d
    jsr rightshift_2d

    lda #$36
    sta $2d
    jsr rightshift_2d
    jsr rightshift_2d

    rts

;; +----------------------------------+
;; |                                  |
;; |    PLAYER STATE                  |
;; |                                  |
;; +----------------------------------+
*=$C000

var_backpackItemModes            = #$00
var_backpackItemTileID           = #$01
var_backpackItemTypeID           = #$02
var_backpackItemIdentifyToTypeID = #$03
var_backpackItemValue            = #$04

playerGoldBalance .byte $00, $00
playerAC          .byte $00
playerWC          .byte $00

bodyTableRowSize = #$05
bodyTableSize = #$04
bodyTable:
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00


backpackRowSize = #$05
backpackSize .byte $00
backpackTable:
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00

;; +----------------------------------+
;; |                                  |
;; |    CURRENT AREA STATE            |
;; |                                  |
;; +----------------------------------+
currentAreaOffsetX  .byte $00
currentAreaOffsetY  .byte $04

playerX .byte $03
playerY .byte $04
prevPlayerX .byte $00
prevPlayerY .byte $00
playerTurnCost .byte $00

screenDirty .byte $00
debugMode .byte $00

;; +----------------------------------+
;; |                                  |
;; |    CURRENT AREA DATA             |
;; |                                  |
;; +----------------------------------+

currentAreaWidth .byte $28
currentAreaHeight .byte $17

drawBufferWidth = $0810
drawBufferHeight = $0811

areaMode:
    .byte %00000000    ; 0/1 = FOV mode on/off
                       ; 0/1 = Randomly generated on/off
                       ; Unused
                       ; Unused
                       ; Unused
                       ; Unused
                       ; Unused
                       ; Unused

currentArea
     .byte $05, $04, $03, $04, $05, $05, $04, $04, $04, $02, $02, $04, $0e, $0c, $05, $05, $05, $05, $04, $05, $05, $05, $05, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
     .byte $05, $05, $0a, $04, $04, $04, $05, $04, $0c, $02, $02, $0d, $04, $04, $04, $04, $04, $01, $05, $05, $04, $05, $04, $04, $04, $04, $04, $05, $04, $04, $04, $05, $0c, $04, $04, $04, $04, $04, $04, $04
     .byte $04, $04, $03, $05, $04, $04, $11, $05, $04, $02, $02, $04, $0b, $04, $04, $04, $11, $05, $04, $05, $11, $05, $05, $04, $05, $04, $04, $04, $04, $04, $05, $05, $04, $04, $04, $04, $04, $04, $04, $04
     .byte $05, $04, $0a, $04, $0b, $01, $05, $04, $0e, $02, $02, $02, $04, $0f, $04, $06, $04, $04, $04, $05, $05, $11, $05, $04, $04, $04, $04, $06, $05, $05, $06, $04, $04, $05, $04, $04, $05, $04, $04, $04
     .byte $04, $04, $0a, $03, $0c, $04, $04, $04, $04, $04, $02, $02, $04, $04, $04, $06, $04, $04, $0d, $0c, $0b, $05, $04, $06, $11, $04, $04, $04, $0d, $0e, $05, $04, $04, $05, $04, $04, $05, $04, $04, $04
     .byte $05, $04, $04, $0a, $05, $04, $04, $01, $04, $04, $02, $02, $04, $06, $05, $04, $04, $05, $04, $05, $04, $04, $05, $04, $04, $05, $04, $06, $06, $05, $04, $06, $04, $04, $05, $04, $04, $04, $04, $04
     .byte $04, $04, $04, $03, $0a, $0d, $0e, $04, $0d, $02, $02, $0c, $04, $04, $11, $0e, $04, $04, $04, $05, $05, $05, $05, $04, $0d, $05, $05, $04, $04, $04, $04, $04, $0c, $04, $05, $04, $05, $04, $04, $04
     .byte $04, $04, $04, $04, $03, $03, $05, $04, $04, $02, $02, $0b, $04, $05, $0b, $0c, $0d, $05, $04, $04, $05, $04, $04, $04, $0c, $04, $05, $04, $04, $04, $0e, $04, $04, $04, $05, $04, $05, $04, $04, $04
     .byte $05, $04, $04, $04, $04, $0a, $03, $0a, $04, $02, $02, $04, $04, $04, $04, $0b, $04, $04, $0b, $04, $05, $05, $05, $04, $0b, $04, $04, $04, $04, $04, $0d, $05, $0b, $04, $04, $04, $04, $04, $04, $04
     .byte $04, $04, $05, $04, $04, $04, $04, $0a, $03, $12, $12, $03, $0a, $03, $03, $04, $04, $04, $05, $05, $05, $04, $05, $04, $04, $04, $0b, $04, $0f, $04, $0c, $05, $04, $04, $04, $04, $04, $04, $04, $04
     .byte $05, $05, $04, $04, $05, $04, $11, $04, $01, $02, $02, $04, $04, $04, $0a, $0a, $03, $0a, $03, $0a, $03, $0a, $03, $03, $04, $04, $0c, $0e, $04, $04, $04, $04, $05, $04, $05, $04, $04, $04, $04, $04
     .byte $05, $04, $05, $04, $06, $04, $04, $0f, $05, $02, $02, $05, $05, $04, $05, $05, $04, $05, $04, $04, $11, $05, $04, $0a, $03, $04, $0d, $04, $04, $04, $04, $04, $05, $04, $05, $04, $05, $04, $04, $04
     .byte $04, $0b, $05, $05, $04, $11, $05, $04, $05, $02, $02, $04, $11, $05, $05, $04, $04, $05, $05, $05, $05, $05, $11, $04, $0a, $04, $08, $08, $08, $08, $08, $08, $05, $04, $04, $04, $05, $04, $04, $04
     .byte $04, $05, $04, $05, $11, $04, $11, $05, $04, $02, $02, $04, $0b, $05, $04, $04, $04, $04, $05, $04, $11, $04, $0b, $04, $03, $04, $08, $08, $08, $08, $08, $08, $04, $04, $0c, $04, $04, $04, $04, $04
     .byte $0d, $04, $04, $05, $04, $05, $11, $04, $02, $02, $04, $05, $0c, $0b, $11, $04, $04, $0f, $04, $05, $05, $04, $0d, $04, $0a, $04, $08, $08, $08, $08, $08, $08, $04, $0e, $0f, $04, $04, $0a, $0a, $0a
     .byte $04, $05, $05, $04, $11, $04, $04, $04, $02, $02, $0f, $0c, $0e, $04, $04, $04, $04, $05, $04, $11, $04, $11, $05, $0b, $0a, $04, $08, $08, $08, $08, $08, $08, $04, $0d, $04, $04, $0a, $03, $04, $04
     .byte $04, $04, $04, $05, $04, $04, $04, $02, $02, $02, $04, $0d, $04, $11, $04, $04, $04, $05, $05, $05, $04, $04, $11, $0c, $03, $04, $07, $07, $07, $09, $07, $07, $04, $04, $04, $04, $0a, $05, $04, $04
     .byte $04, $04, $04, $04, $04, $02, $02, $02, $02, $04, $05, $04, $06, $05, $05, $04, $04, $04, $04, $04, $05, $05, $04, $04, $0a, $0a, $04, $06, $04, $0a, $04, $05, $04, $04, $0a, $03, $03, $04, $04, $04
     .byte $0b, $04, $04, $04, $02, $02, $02, $02, $04, $04, $04, $04, $06, $06, $11, $05, $04, $04, $05, $05, $04, $04, $04, $04, $04, $03, $03, $0a, $0a, $03, $03, $0a, $03, $03, $0a, $04, $04, $05, $05, $04
     .byte $04, $05, $04, $04, $02, $02, $04, $04, $04, $0b, $04, $04, $04, $04, $11, $05, $01, $04, $05, $04, $05, $04, $11, $04, $04, $04, $04, $04, $0f, $04, $04, $04, $04, $04, $04, $05, $05, $04, $05, $04
     .byte $0d, $04, $04, $02, $02, $04, $04, $0d, $0e, $0c, $05, $05, $05, $04, $05, $0b, $0f, $10, $04, $04, $05, $04, $05, $04, $04, $0b, $0e, $0d, $05, $06, $04, $0f, $04, $05, $04, $05, $05, $04, $05, $04
     .byte $04, $05, $01, $02, $02, $04, $04, $04, $04, $0d, $04, $04, $04, $11, $04, $0e, $0c, $04, $05, $01, $04, $04, $04, $04, $0b, $0c, $0c, $0e, $04, $05, $04, $04, $04, $04, $05, $04, $05, $04, $04, $04
     .byte $04, $04, $04, $02, $02, $04, $04, $04, $04, $04, $04, $04, $05, $04, $04, $04, $0d, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04

var_triggerXPos     = #$00
var_triggerYPos     = #$01
var_triggerType     = #$02
var_triggerTgtAreaLo = #$03
var_triggerTgtAreaHi = #$04
var_triggerTgtXPos = #$05
var_triggerTgtIndex = #$05
var_triggerTgtYPos = #$06

triggerTableRowSize = #$07
triggerTableSize
     .byte $01
triggerTable
     .byte $1d  ; Trigger X
     .byte $10  ; Trigger Y
     .byte $01  ; Trigger Type (01 = Teleport to new area)
     .byte <(houseArea)
     .byte >(houseArea)
     .byte $04
     .byte $06
     .byte $1d  ; Trigger X
     .byte $10  ; Trigger Y
     .byte $01  ; Trigger Type (01 = Teleport to new area)
     .byte <(houseArea)
     .byte >(houseArea)
     .byte $04
     .byte $06

; npcBits
; 0
; 1
; 2
; 3
; 4
; 5
; 6 - Hostile On/Off
; 7 - NPC On/Off

var_npcModes     = #$00
var_npcXPos      = #$01
var_npcYPos      = #$02
var_npcTileID    = #$03
var_npcSpritePtr = #$04
var_npcNamePtrLo = #$05
var_npcNamePtrHi = #$06
var_npcCurrentHP = #$07
var_npcMode      = #$08 ; 00 = Wait
                        ; 01 = Random Walk
                        ; 02 = Follow/Attack Player
                        ; 03 = Avoid Player
var_npcTargetX   = #$09
var_npcTargetY   = #$0a
var_npcMoveCost  = #$0b
var_npcCurrentAP = #$0c

npcTableRowSize = #$0d
npcTableSize .byte $00
npcTable
     .byte %10000000
     .byte $0f, $08         ;; X and Y pos
     .byte $20              ;; Tile ID
     .byte $89              ;; Sprite pointer   $00 = off
     .byte $00, $00         ;; Name pointer
     .byte $12              ;; HP
     .byte $01              ;; Mode
     .byte 0, 0             ;; Target X and Y pos
     .byte 11               ;; Movement Cost
     .byte 11               ;; AP

     .byte %10000000
     .byte $04, $04         ;; X and Y pos
     .byte $20              ;; Tile ID
     .byte $89              ;; Sprite pointer   $00 = off
     .byte $00, $00         ;; Name pointer
     .byte $12              ;; HP
     .byte $01              ;; Mode
     .byte 0, 0             ;; Target X and Y pos
     .byte 11               ;; Movement Cost
     .byte 11               ;; AP

     .byte %10000000
     .byte $12, $09         ;; X and Y pos
     .byte $20              ;; Tile ID
     .byte $89              ;; Sprite pointer   $00 = off
     .byte $00, $00         ;; Name pointer
     .byte $12              ;; HP
     .byte $00              ;; Mode
     .byte 0, 0             ;; Target X and Y pos
     .byte 11               ;; Movement Cost
     .byte 11               ;; AP

     .byte %10000000
     .byte $0e, $13         ;; X and Y pos
     .byte $20              ;; Tile ID
     .byte $89              ;; Sprite pointer   $00 = off
     .byte $00, $00         ;; Name pointer
     .byte $12              ;; HP
     .byte $00              ;; Mode
     .byte 0, 0             ;; Target X and Y pos
     .byte 11               ;; Movement Cost
     .byte 11               ;; AP

     .byte %00000000
     .byte $0f, $08         ;; X and Y pos
     .byte $20              ;; Tile ID
     .byte $89              ;; Sprite pointer   $00 = off
     .byte $00, $00         ;; Name pointer
     .byte $12              ;; HP
     .byte $00              ;; Mode
     .byte 0, 0             ;; Target X and Y pos
     .byte 11               ;; Movement Cost
     .byte 11               ;; AP

     .byte %00000000
     .byte $0f, $08         ;; X and Y pos
     .byte $20              ;; Tile ID
     .byte $89              ;; Sprite pointer   $00 = off
     .byte $00, $00         ;; Name pointer
     .byte $12              ;; HP
     .byte $00              ;; Mode
     .byte 0, 0             ;; Target X and Y pos
     .byte 11               ;; Movement Cost
     .byte 11               ;; AP

     .byte %00000000
     .byte $0f, $08         ;; X and Y pos
     .byte $20              ;; Tile ID
     .byte $89              ;; Sprite pointer   $00 = off
     .byte $00, $00         ;; Name pointer
     .byte $12              ;; HP
     .byte $00              ;; Mode
     .byte 0, 0             ;; Target X and Y pos
     .byte 11               ;; Movement Cost
     .byte 11               ;; AP

     .byte %00000000
     .byte $0f, $08         ;; X and Y pos
     .byte $20              ;; Tile ID
     .byte $89              ;; Sprite pointer   $00 = off
     .byte $00, $00         ;; Name pointer
     .byte $12              ;; HP
     .byte $00              ;; Mode
     .byte 0, 0             ;; Target X and Y pos
     .byte 11               ;; Movement Cost
     .byte 11               ;; AP

     .byte %00000000
     .byte $0f, $08         ;; X and Y pos
     .byte $20              ;; Tile ID
     .byte $89              ;; Sprite pointer   $00 = off
     .byte $00, $00         ;; Name pointer
     .byte $12              ;; HP
     .byte $00              ;; Mode
     .byte 0, 0             ;; Target X and Y pos
     .byte 11               ;; Movement Cost
     .byte 11               ;; AP

     .byte %00000000
     .byte $0f, $08         ;; X and Y pos
     .byte $20              ;; Tile ID
     .byte $89              ;; Sprite pointer   $00 = off
     .byte $00, $00         ;; Name pointer
     .byte $12              ;; HP
     .byte $00              ;; Mode
     .byte 0, 0             ;; Target X and Y pos
     .byte 11               ;; Movement Cost
     .byte 11               ;; AP

var_itemModes     = #$00 ; Bit 7 - On/Off
                         ; Bit 6 - Is identified? On/Off
                         ; Bit 5 - Unused
                         ; Bits 0-4 - Item Category (Int)

;; 00 - xxx00000        - Pieces of Gold
;; 01 - xxx00001        - Scroll
;; 02 - xxx00010        - Potion
;; 03 - xxx00011        - Wand
;; 04 - xxx00100        - (Unused)
;; 05 - xxx00101        - (Unused)
;; 06 - xxx00110        - Melee Weapon
;; 07 - xxx00111        - Ranged Weapon
;; 08 - xxx01000        - (Unused)
;; 09 - xxx01001        - (Unused)
;; 0a - xxx01010        - Shield
;; 0b - xxx01011        - Helmet
;; 0c - xxx01100        - Armor

var_itemXPos             = #$01
var_itemYPos             = #$02
var_itemTileID           = #$03
var_itemTypeID           = #$04
var_itemIdentifyToTypeID = #$05
var_itemValue            = #$06    ; Amount if amount bit set.
                            ; Actual Item ID if unidentified bit set.

itemTableRowSize .byte $07
itemTableSize .byte $00
itemTable

     .byte %11000000
     .byte $10, $0a                ;; X and Y pos
     .byte $32                     ;; Tile ID
     .word itemname_PIECES_OF_GOLD ;; Name pointer
     .byte $15                     ;; Amount

     .byte %10100000
     .byte $13, $08         ;; X and Y pos
     .byte $30              ;; Tile ID
     .word itemname_SCROLL  ;; Name pointer
     .byte $00              ;; Actual Type

     .byte %10100000
     .byte $13, $08         ;; X and Y pos;
     .byte $31              ;; Tile ID
     .word itemname_POTION  ;; Name pointer
     .byte $01              ;; Actual Type

     .byte %11000000
     .byte $18, $13         ;; X and Y pos
     .byte $32              ;; Tile ID
     .word itemname_PIECES_OF_GOLD ;; Name pointer
     .byte $09                     ;; Amount

     .byte %11000000
     .byte $18, $14         ;; X and Y pos
     .byte $32              ;; Tile ID
     .word itemname_PIECES_OF_GOLD ;; Name pointer
     .byte $19                     ;; Amount

     .byte %11000000
     .byte $18, $14         ;; X and Y pos
     .byte $32              ;; Tile ID
     .word itemname_PIECES_OF_GOLD ;; Name pointer
     .byte $19                     ;; Amount

     .byte %11000000
     .byte $18, $14         ;; X and Y pos
     .byte $32              ;; Tile ID
     .word itemname_PIECES_OF_GOLD ;; Name pointer
     .byte $19                     ;; Amount

     .byte %11000000
     .byte $18, $14         ;; X and Y pos
     .byte $32              ;; Tile ID
     .word itemname_PIECES_OF_GOLD ;; Name pointer
     .byte $19                     ;; Amount

     .byte %11000000
     .byte $18, $14         ;; X and Y pos
     .byte $32              ;; Tile ID
     .word itemname_PIECES_OF_GOLD ;; Name pointer
     .byte $19                     ;; Amount

     .byte %11000000
     .byte $18, $14         ;; X and Y pos
     .byte $32              ;; Tile ID
     .word itemname_PIECES_OF_GOLD ;; Name pointer
     .byte $19                     ;; Amount

;; +----------------------------------+
;; |                                  |
;; |    TEST AREAS                    |
;; |                                  |
;; +----------------------------------+
*=$7300
.include "testareas.asm"

;; +----------------------------------+
;; |                                  |
;; |    CURRENT VIEWPORT BUFFERS      |
;; |                                  |
;; +----------------------------------+

*=$4000
fovBuffer
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00

screenBuffer
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00

;; +----------------------------------+
;; |                                  |
;; |    CURRENT TILESET               |
;; |                                  |
;; +----------------------------------+


tileChar1
     .byte $48   ;; Nothing/Black   $00
     .byte $47   ;; Rocks           $01
     .byte $46   ;; Water           $02
     .byte $44   ;; Road            $03
     .text " "   ;; Background      $04
     .byte $43   ;; Tree            $05
     .byte $4b   ;; Dead tree       $06
     .byte $4d   ;; Red wall        $07
     .byte $4e   ;; Red roof        $08
     .byte $51   ;; Door            $09
     .byte $45   ;; RoadInverse     $0a
     .byte $20   ;; Grass Half 1    $0b
     .byte $54   ;; Grass Half 2    $0c
     .byte $54   ;; Grass Half 1    $0d
     .byte $53   ;; Grass Half 2    $0e
     .byte $20   ;; Flowers Red     $0f
     .byte $56   ;; Flowers Yellow  $10
     .byte $57   ;; Tree Variant    $11
     .byte $59   ;; Bridge          $12
     .byte $00   ;;                 $13
     .byte $00   ;;                 $14
     .byte $00   ;;                 $15
     .byte $00   ;;                 $16
     .byte $00   ;;                 $17
     .byte $00   ;;                 $18
     .byte $00   ;;                 $19
     .byte $00   ;;                 $1a
     .byte $00   ;;                 $1b
     .byte $00   ;;                 $1c
     .byte $00   ;;                 $1d
     .byte $00   ;;                 $1e
     .byte $00   ;;                 $1f
     .byte $00   ;;                 $20
     .byte $00   ;;                 $21
     .byte $00   ;;                 $22
     .byte $00   ;;                 $23
     .byte $00   ;;                 $24
     .byte $00   ;;                 $25
     .byte $00   ;;                 $26
     .byte $00   ;;                 $27
     .byte $00   ;;                 $28
     .byte $00   ;;                 $29
     .byte $00   ;;                 $2a
     .byte $00   ;;                 $2b
     .byte $00   ;;                 $2c
     .byte $00   ;;                 $2d
     .byte $00   ;;                 $2e
     .byte $00   ;;                 $2f
; Npc Tiles
     .byte $80   ;; Rat             $30
     .byte $82   ;; Skeleton        $31
     .byte $84   ;; Kobold          $32
     .byte $86   ;; Orc             $33
     .byte $8a   ;; Giant Spider    $34
     .byte $88   ;; Viper           $35
     .byte $00   ;; Unused monster  $36
     .byte $00   ;; Unused monster  $37
     .byte $00   ;; Unused monster  $38
     .byte $00   ;; Unused monster  $39
     .byte $00   ;; Unused monster  $3a
     .byte $00   ;; Unused monster  $3b
     .byte $00   ;; Unused monster  $3c
     .byte $00   ;; Unused monster  $3d
     .byte $00   ;; Unused monster  $3e
     .byte $00   ;; Unused monster  $3f
     .byte $c0   ;; Scroll          $40
     .byte $c2   ;; Potion (blue)   $41
     .byte $c4   ;; Coins           $42
     .byte $c6   ;; Sword           $43
     .byte $c8   ;; Armor           $44
     .byte $c2   ;; Potion (red)    $45
     .byte $c2   ;; Potion (green)  $46
     .byte $c3   ;; Potion (purple) $47
     .byte $c5   ;; Shortsword      $48
     .byte $c6   ;; Longsword       $49
     .byte $ca   ;; Helmet          $4a
     .byte $c2   ;; Potion (turq)   $4b
     .byte $c2   ;; Potion (white)  $4c
     .byte $cc   ;; Round Shield    $4d

tileChar2
     .byte $48   ;; Nothing/Black   $00
     .byte $20   ;; Rocks           $01
     .byte $46   ;; Water           $02
     .byte $45   ;; Road            $03
     .text " "   ;; Background      $04
     .byte $42   ;; Tree            $05
     .byte $4c   ;; Dead tree       $06
     .byte $4d   ;; Red wall        $07
     .byte $4e   ;; Red roof        $08
     .byte $52   ;; Door            $09
     .byte $44   ;; RoadInverse     $0a
     .byte $53   ;; Grass Half 1    $0b
     .byte $53   ;; Grass Half 2    $0c
     .byte $20   ;; Grass Half 1    $0d
     .byte $54   ;; Grass Half 2    $0e
     .byte $56   ;; Flowers Red     $0f
     .byte $20   ;; Flowers Yellow  $10
     .byte $58   ;; Tree Variant    $11
     .byte $59   ;; Bridge          $12
     .byte $00   ;;                 $13
     .byte $00   ;;                 $14
     .byte $00   ;;                 $15
     .byte $00   ;;                 $16
     .byte $00   ;;                 $17
     .byte $00   ;;                 $18
     .byte $00   ;;                 $19
     .byte $00   ;;                 $1a
     .byte $00   ;;                 $1b
     .byte $00   ;;                 $1c
     .byte $00   ;;                 $1d
     .byte $00   ;;                 $1e
     .byte $00   ;;                 $1f
     .byte $00   ;;                 $20
     .byte $00   ;;                 $21
     .byte $00   ;;                 $22
     .byte $00   ;;                 $23
     .byte $00   ;;                 $24
     .byte $00   ;;                 $25
     .byte $00   ;;                 $26
     .byte $00   ;;                 $27
     .byte $00   ;;                 $28
     .byte $00   ;;                 $29
     .byte $00   ;;                 $2a
     .byte $00   ;;                 $2b
     .byte $00   ;;                 $2c
     .byte $00   ;;                 $2d
     .byte $00   ;;                 $2e
     .byte $00   ;;                 $2f
; Npc Tiles
     .byte $81   ;; Rat             $30
     .byte $83   ;; Skeleton        $31
     .byte $85   ;; Kobold          $32
     .byte $87   ;; Orc             $33
     .byte $8b   ;; Giant Spider    $34
     .byte $89   ;; Viper           $35
     .byte $00   ;; Unused monster  $36
     .byte $00   ;; Unused monster  $37
     .byte $00   ;; Unused monster  $38
     .byte $00   ;; Unused monster  $39
     .byte $00   ;; Unused monster  $3a
     .byte $00   ;; Unused monster  $3b
     .byte $00   ;; Unused monster  $3c
     .byte $00   ;; Unused monster  $3d
     .byte $00   ;; Unused monster  $3e
     .byte $00   ;; Unused monster  $3f
; Items
     .byte $c1   ;; Scroll          $40
     .byte $c3   ;; Potion          $41
     .byte $20   ;; Coins           $42
     .byte $c7   ;; Sword           $43
     .byte $c9   ;; Armor           $44
     .byte $c3   ;; Potion (red)    $45
     .byte $c3   ;; Potion (green)  $46
     .byte $c3   ;; Potion (purple) $47
     .byte $c7   ;; Shortsword      $48
     .byte $c7   ;; Longsword       $49
     .byte $cb   ;; Helmet          $4a
     .byte $c3   ;; Potion (turq)   $4b
     .byte $c3   ;; Potion (white)  $4c
     .byte $cd   ;; Round Shield    $4d

tileChar3
     .byte $48 	 ;; Nothing/Black   $00
	 .byte $20   ;; Rocks           $01
     .byte $46   ;; Water           $02
     .byte $45   ;; Road            $03
     .text " "   ;; Background      $04
     .byte $40   ;; Tree            $05
     .byte $49   ;; Dead tree       $06
     .byte $4d   ;; Red wall        $07
     .byte $4e   ;; Red roof        $08
     .byte $4f   ;; Door            $09
     .byte $44   ;; RoadInverse     $0a
     .byte $54   ;; Grass Half 1    $0b
     .byte $53   ;; Grass Half 2    $0c
     .byte $20   ;; Grass Half 1    $0d
     .byte $54   ;; Grass Half 2    $0e
     .byte $20   ;; Flowers Red     $0f
     .byte $55   ;; Flowers Yellow  $10
     .byte $40   ;; Tree Variant    $11
     .byte $59   ;; Bridge          $12
     .byte $00   ;;                 $13
     .byte $00   ;;                 $14
     .byte $00   ;;                 $15
     .byte $00   ;;                 $16
     .byte $00   ;;                 $17
     .byte $00   ;;                 $18
     .byte $00   ;;                 $19
     .byte $00   ;;                 $1a
     .byte $00   ;;                 $1b
     .byte $00   ;;                 $1c
     .byte $00   ;;                 $1d
     .byte $00   ;;                 $1e
     .byte $00   ;;                 $1f
     .byte $00   ;;                 $20
     .byte $00   ;;                 $21
     .byte $00   ;;                 $22
     .byte $00   ;;                 $23
     .byte $00   ;;                 $24
     .byte $00   ;;                 $25
     .byte $00   ;;                 $26
     .byte $00   ;;                 $27
     .byte $00   ;;                 $28
     .byte $00   ;;                 $29
     .byte $00   ;;                 $2a
     .byte $00   ;;                 $2b
     .byte $00   ;;                 $2c
     .byte $00   ;;                 $2d
     .byte $00   ;;                 $2e
     .byte $00   ;;                 $2f
; Npc Tiles
     .byte $a0   ;; Rat             $30
     .byte $a2   ;; Skeleton        $31
     .byte $a4   ;; Kobold          $32
     .byte $a6   ;; Orc             $33
     .byte $aa   ;; Giant Spider    $34
     .byte $a8   ;; Viper           $35
     .byte $00   ;; Unused monster  $36
     .byte $00   ;; Unused monster  $37
     .byte $00   ;; Unused monster  $38
     .byte $00   ;; Unused monster  $39
     .byte $00   ;; Unused monster  $3a
     .byte $00   ;; Unused monster  $3b
     .byte $00   ;; Unused monster  $3c
     .byte $00   ;; Unused monster  $3d
     .byte $00   ;; Unused monster  $3e
     .byte $00   ;; Unused monster  $3f
; Items
     .byte $e0   ;; Scroll          $40
     .byte $e2   ;; Potion          $41
     .byte $e4   ;; Coins           $42
     .byte $e6   ;; Sword           $43
     .byte $e8   ;; Armor           $44
     .byte $e2   ;; Potion (red)    $45
     .byte $e2   ;; Potion (green)  $46
     .byte $e2   ;; Potion (purple) $47
     .byte $e6   ;; Shortsword      $48
     .byte $e6   ;; Longsword       $49
     .byte $ea   ;; Helmet          $4a
     .byte $e2   ;; Potion (turq)   $4b
     .byte $e2   ;; Potion (white)  $4c
     .byte $ec   ;; Round Shield    $4d

tileChar4
     .byte $48   ;; Nothing/Black   $00
     .byte $47   ;; Rocks           $01
     .byte $46   ;; Water           $02
     .byte $44   ;; Road            $03
     .text " "   ;; Background      $04
     .byte $41   ;; Tree            $05
     .byte $4a   ;; Dead tree       $06
     .byte $4d   ;; Red wall        $07
     .byte $4e   ;; Red roof        $08
     .byte $50   ;; Door            $09
     .byte $45   ;; RoadInverse     $0a
     .byte $20   ;; Grass Half 1    $0b
     .byte $54   ;; Grass Half 2    $0c
     .byte $53   ;; Grass Half 1    $0d
     .byte $53   ;; Grass Half 2    $0e
     .byte $55   ;; Flowers Red     $0f
     .byte $20   ;; Flowers Yellow  $10
     .byte $41   ;; Tree Variant    $11
     .byte $59   ;; Bridge          $12
     .byte $00   ;;                 $13
     .byte $00   ;;                 $14
     .byte $00   ;;                 $15
     .byte $00   ;;                 $16
     .byte $00   ;;                 $17
     .byte $00   ;;                 $18
     .byte $00   ;;                 $19
     .byte $00   ;;                 $1a
     .byte $00   ;;                 $1b
     .byte $00   ;;                 $1c
     .byte $00   ;;                 $1d
     .byte $00   ;;                 $1e
     .byte $00   ;;                 $1f
     .byte $00   ;;                 $20
     .byte $00   ;;                 $21
     .byte $00   ;;                 $22
     .byte $00   ;;                 $23
     .byte $00   ;;                 $24
     .byte $00   ;;                 $25
     .byte $00   ;;                 $26
     .byte $00   ;;                 $27
     .byte $00   ;;                 $28
     .byte $00   ;;                 $29
     .byte $00   ;;                 $2a
     .byte $00   ;;                 $2b
     .byte $00   ;;                 $2c
     .byte $00   ;;                 $2d
     .byte $00   ;;                 $2e
     .byte $00   ;;                 $2f
	; Npc Tiles
     .byte $a1   ;; Rat             $30
     .byte $a3   ;; Skeleton        $31
     .byte $a5   ;; Kobold          $32
     .byte $a7   ;; Orc             $33
     .byte $ab   ;; Giant Spider    $34
     .byte $a9   ;; Viper           $35
     .byte $00   ;; Unused monster  $36
     .byte $00   ;; Unused monster  $37
     .byte $00   ;; Unused monster  $38
     .byte $00   ;; Unused monster  $39
     .byte $00   ;; Unused monster  $3a
     .byte $00   ;; Unused monster  $3b
     .byte $00   ;; Unused monster  $3c
     .byte $00   ;; Unused monster  $3d
     .byte $00   ;; Unused monster  $3e
     .byte $00   ;; Unused monster  $3f
     ; Items
     .byte $e1   ;; Scroll          $40
     .byte $e3   ;; Potion          $41
     .byte $e5   ;; Coins           $42
     .byte $e7   ;; Sword           $43
     .byte $e9   ;; Armor           $44
     .byte $e3   ;; Potion (red)    $45
     .byte $e3   ;; Potion (green)  $46
     .byte $e3   ;; Potion (purple) $47
     .byte $e7   ;; Shortsword      $48
     .byte $e7   ;; Longsword       $49
     .byte $eb   ;; Helmet          $4a
     .byte $e3   ;; Potion (turq)   $4b
     .byte $e3   ;; Potion (white)  $4c
     .byte $ed   ;; Round Shield    $4d

tileCharColor1
     .byte $00   ;; Nothing / Black
     .byte $01   ;; Rocks - Hires white
     .byte $1e   ;; Water - blue
     .byte $1d   ;; Road - green
     .byte $1d   ;; Background - N/A
     .byte $1d   ;; Tree - green
     .byte $1a   ;; Dead tree - red
     .byte $1a   ;; Red wall - red
     .byte $1a   ;; Red roof - red
     .byte $08   ;; Door
     .byte $1d   ;; Road - green
     .byte $00   ;; Grass
     .byte $00   ;; Grass
     .byte $00   ;; Grass
     .byte $00   ;; Grass
     .byte $00   ;; Flowers Yellow
     .byte $07   ;; Flowers Red
     .byte $1d   ;; Tree variant
     .byte $08   ;; Bridge
     .byte $00   ;;                 $13
     .byte $00   ;;                 $14
     .byte $00   ;;                 $15
     .byte $00   ;;                 $16
     .byte $00   ;;                 $17
     .byte $00   ;;                 $18
     .byte $00   ;;                 $19
     .byte $00   ;;                 $1a
     .byte $00   ;;                 $1b
     .byte $00   ;;                 $1c
     .byte $00   ;;                 $1d
     .byte $00   ;;                 $1e
     .byte $00   ;;                 $1f
     .byte $00   ;;                 $20
     .byte $00   ;;                 $21
     .byte $00   ;;                 $22
     .byte $00   ;;                 $23
     .byte $00   ;;                 $24
     .byte $00   ;;                 $25
     .byte $00   ;;                 $26
     .byte $00   ;;                 $27
     .byte $00   ;;                 $28
     .byte $00   ;;                 $29
     .byte $00   ;;                 $2a
     .byte $00   ;;                 $2b
     .byte $00   ;;                 $2c
     .byte $00   ;;                 $2d
     .byte $00   ;;                 $2e
     .byte $00   ;;                 $2f
     .byte $1a   ;; Rat             $30
     .byte $09   ;; Skeleton        $31
     .byte $0d   ;; Kobold          $32
     .byte $1a   ;; Orc             $33
     .byte $1a   ;; Giant Spider    $34
     .byte $0d   ;; Viper           $35
     .byte $00   ;; Unused monster  $36
     .byte $00   ;; Unused monster  $37
     .byte $00   ;; Unused monster  $38
     .byte $00   ;; Unused monster  $39
     .byte $00   ;; Unused monster  $3a
     .byte $00   ;; Unused monster  $3b
     .byte $00   ;; Unused monster  $3c
     .byte $00   ;; Unused monster  $3d
     .byte $00   ;; Unused monster  $3e
     .byte $00   ;; Unused monster  $3f
; Items
     .byte $0f   ;; Scroll          $40
     .byte $0e   ;; Potion          $41
     .byte $0f   ;; Coins           $42
     .byte $0a   ;; Sword           $43
     .byte $0a   ;; Armor           $44
     .byte $0a   ;; Potion (red)    $45
     .byte $0d   ;; Potion (green)  $46
     .byte $0c   ;; Potion (purple) $47
     .byte $0a   ;; Shortsword      $48
     .byte $0a   ;; Longsword       $49
     .byte $0a   ;; Helmet          $4a
     .byte $0b   ;; Potion (turq)   $4b
     .byte $09   ;; Potion (white)  $4c
     .byte $0a   ;; Round Shield    $4d

tileCharColor2
     .byte $00   ;; Nothing / Black
     .byte $01   ;; Rocks - Hires white
     .byte $1e   ;; Water - blue
     .byte $1d   ;; Road - green
     .byte $1d   ;; Background - N/A
     .byte $1d   ;; Tree - green
     .byte $1a   ;; Dead tree - red
     .byte $1a   ;; Red wall - red
     .byte $1a   ;; Red roof - red
     .byte $08   ;; Door
     .byte $1d   ;; Road - green
     .byte $00   ;; Grass
     .byte $00   ;; Grass
     .byte $00   ;; Grass
     .byte $00   ;; Grass
     .byte $02   ;; Flowers Yellow
     .byte $00   ;; Flowers Red
     .byte $1d   ;; Tree variant
     .byte $08   ;; Bridge
     .byte $00   ;;                 $13
     .byte $00   ;;                 $14
     .byte $00   ;;                 $15
     .byte $00   ;;                 $16
     .byte $00   ;;                 $17
     .byte $00   ;;                 $18
     .byte $00   ;;                 $19
     .byte $00   ;;                 $1a
     .byte $00   ;;                 $1b
     .byte $00   ;;                 $1c
     .byte $00   ;;                 $1d
     .byte $00   ;;                 $1e
     .byte $00   ;;                 $1f
     .byte $00   ;;                 $20
     .byte $00   ;;                 $21
     .byte $00   ;;                 $22
     .byte $00   ;;                 $23
     .byte $00   ;;                 $24
     .byte $00   ;;                 $25
     .byte $00   ;;                 $26
     .byte $00   ;;                 $27
     .byte $00   ;;                 $28
     .byte $00   ;;                 $29
     .byte $00   ;;                 $2a
     .byte $00   ;;                 $2b
     .byte $00   ;;                 $2c
     .byte $00   ;;                 $2d
     .byte $00   ;;                 $2e
     .byte $00   ;;                 $2f
     .byte $1a   ;; Rat             $30
     .byte $01   ;; Skeleton        $31
     .byte $0d   ;; Kobold          $32
     .byte $1a   ;; Orc             $33
     .byte $1a   ;; Giant Spider    $34
     .byte $0d   ;; Viper           $35
     .byte $00   ;; Unused monster  $36
     .byte $00   ;; Unused monster  $37
     .byte $00   ;; Unused monster  $38
     .byte $00   ;; Unused monster  $39
     .byte $00   ;; Unused monster  $3a
     .byte $00   ;; Unused monster  $3b
     .byte $00   ;; Unused monster  $3c
     .byte $00   ;; Unused monster  $3d
     .byte $00   ;; Unused monster  $3e
     .byte $00   ;; Unused monster  $3f
; Items
     .byte $0f   ;; Scroll          $40
     .byte $0e   ;; Potion          $41
     .byte $0f   ;; Coins           $42
     .byte $0a   ;; Sword           $43
     .byte $0a   ;; Armor           $44
     .byte $0a   ;; Potion (red)    $45
     .byte $0d   ;; Potion (green)  $46
     .byte $0c   ;; Potion (purple) $47
     .byte $0a   ;; Shortsword      $48
     .byte $0a   ;; Longsword       $49
     .byte $0a   ;; Helmet          $4a
     .byte $0b   ;; Potion (turq)   $4b
     .byte $09   ;; Potion (white)  $4c
     .byte $0a   ;; Round Shield    $4d

tileCharColor3
     .byte $00   ;; Nothing / Black
     .byte $01   ;; Rocks - Hires white
     .byte $1e   ;; Water - blue
     .byte $1d   ;; Road - green
     .byte $1d   ;; Background - N/A
     .byte $1d   ;; Tree - green
     .byte $1a   ;; Dead tree - red
     .byte $1a   ;; Red wall - red
     .byte $1a   ;; Red roof - red
     .byte $08   ;; Door
     .byte $1d   ;; Road - green
     .byte $00   ;; Grass
     .byte $00   ;; Grass
     .byte $00   ;; Grass
     .byte $00   ;; Grass
     .byte $00   ;; Flowers Yellow
     .byte $00   ;; Flowers Red
     .byte $1d   ;; Tree variant
     .byte $08   ;; Bridge
     .byte $00   ;;                 $13
     .byte $00   ;;                 $14
     .byte $00   ;;                 $15
     .byte $00   ;;                 $16
     .byte $00   ;;                 $17
     .byte $00   ;;                 $18
     .byte $00   ;;                 $19
     .byte $00   ;;                 $1a
     .byte $00   ;;                 $1b
     .byte $00   ;;                 $1c
     .byte $00   ;;                 $1d
     .byte $00   ;;                 $1e
     .byte $00   ;;                 $1f
     .byte $00   ;;                 $20
     .byte $00   ;;                 $21
     .byte $00   ;;                 $22
     .byte $00   ;;                 $23
     .byte $00   ;;                 $24
     .byte $00   ;;                 $25
     .byte $00   ;;                 $26
     .byte $00   ;;                 $27
     .byte $00   ;;                 $28
     .byte $00   ;;                 $29
     .byte $00   ;;                 $2a
     .byte $00   ;;                 $2b
     .byte $00   ;;                 $2c
     .byte $00   ;;                 $2d
     .byte $00   ;;                 $2e
     .byte $00   ;;                 $2f
     .byte $1a   ;; Rat             $30
     .byte $09   ;; Skeleton        $31
     .byte $0d   ;; Kobold          $32
     .byte $1a   ;; Orc             $33
     .byte $1a   ;; Giant Spider    $34
     .byte $0d   ;; Viper           $35
     .byte $00   ;; Unused monster  $36
     .byte $00   ;; Unused monster  $37
     .byte $00   ;; Unused monster  $38
     .byte $00   ;; Unused monster  $39
     .byte $00   ;; Unused monster  $3a
     .byte $00   ;; Unused monster  $3b
     .byte $00   ;; Unused monster  $3c
     .byte $00   ;; Unused monster  $3d
     .byte $00   ;; Unused monster  $3e
     .byte $00   ;; Unused monster  $3f
; Items
     .byte $0f   ;; Scroll          $40
     .byte $0e   ;; Potion          $41
     .byte $0f   ;; Coins           $42
     .byte $0a   ;; Sword           $43
     .byte $0a   ;; Armor           $44
     .byte $0a   ;; Potion (red)    $45
     .byte $0d   ;; Potion (green)  $46
     .byte $0c   ;; Potion (purple) $47
     .byte $0a   ;; Shortsword      $48
     .byte $0a   ;; Longsword       $49
     .byte $0a   ;; Helmet          $4a
     .byte $0b   ;; Potion (turq)   $4b
     .byte $09   ;; Potion (white)  $4c
     .byte $0a   ;; Round Shield    $4d

tileCharColor4
     .byte $00   ;; Nothing / Black
     .byte $01   ;; Rocks - Hires white
     .byte $1e   ;; Water - blue
     .byte $1d   ;; Road - green
     .byte $1d   ;; Background - N/A
     .byte $1d   ;; Tree - green
     .byte $1a   ;; Dead tree - red
     .byte $1a   ;; Red wall - red
     .byte $1a   ;; Red roof - red
     .byte $08   ;; Door
     .byte $1d   ;; Road - green
     .byte $00   ;; Grass
     .byte $00   ;; Grass
     .byte $00   ;; Grass
     .byte $00   ;; Grass
     .byte $00   ;; Flowers Yellow
     .byte $00   ;; Flowers Red
     .byte $1d   ;; Tree variant
     .byte $08   ;; Bridge
     .byte $00   ;;                 $13
     .byte $00   ;;                 $14
     .byte $00   ;;                 $15
     .byte $00   ;;                 $16
     .byte $00   ;;                 $17
     .byte $00   ;;                 $18
     .byte $00   ;;                 $19
     .byte $00   ;;                 $1a
     .byte $00   ;;                 $1b
     .byte $00   ;;                 $1c
     .byte $00   ;;                 $1d
     .byte $00   ;;                 $1e
     .byte $00   ;;                 $1f
     .byte $00   ;;                 $20
     .byte $00   ;;                 $21
     .byte $00   ;;                 $22
     .byte $00   ;;                 $23
     .byte $00   ;;                 $24
     .byte $00   ;;                 $25
     .byte $00   ;;                 $26
     .byte $00   ;;                 $27
     .byte $00   ;;                 $28
     .byte $00   ;;                 $29
     .byte $00   ;;                 $2a
     .byte $00   ;;                 $2b
     .byte $00   ;;                 $2c
     .byte $00   ;;                 $2d
     .byte $00   ;;                 $2e
     .byte $00   ;;                 $2f
     .byte $1a   ;; Rat             $30
     .byte $01   ;; Skeleton        $31
     .byte $0d   ;; Kobold          $32
     .byte $1a   ;; Orc             $33
     .byte $1a   ;; Giant Spider    $34
     .byte $0d   ;; Viper           $35
     .byte $00   ;; Unused monster  $36
     .byte $00   ;; Unused monster  $37
     .byte $00   ;; Unused monster  $38
     .byte $00   ;; Unused monster  $39
     .byte $00   ;; Unused monster  $3a
     .byte $00   ;; Unused monster  $3b
     .byte $00   ;; Unused monster  $3c
     .byte $00   ;; Unused monster  $3d
     .byte $00   ;; Unused monster  $3e
     .byte $00   ;; Unused monster  $3f
; Items
     .byte $0f   ;; Scroll          $40
     .byte $0e   ;; Potion          $41
     .byte $0f   ;; Coins           $42
     .byte $0a   ;; Sword           $43
     .byte $0a   ;; Armor           $44
     .byte $0a   ;; Potion (red)    $45
     .byte $0d   ;; Potion (green)  $46
     .byte $0c   ;; Potion (purple) $47
     .byte $0a   ;; Shortsword      $48
     .byte $0a   ;; Longsword       $49
     .byte $0a   ;; Helmet          $4a
     .byte $0b   ;; Potion (turq)   $4b
     .byte $09   ;; Potion (white)  $4c
     .byte $0a   ;; Round Shield    $4d

tileProps:
     .byte %00000000          ;; Nothing / Black. Not passable.    Block Sight
     .byte %11000000          ;; Rocks.           Passable         See-through
     .byte %01000000          ;; Water            Not passable     See-through
     .byte %11000000          ;; Road             Passable         See-through
     .byte %11000000          ;; Background       Passable         See-through
     .byte %00000000          ;; Tree             Not passable     Block sight
     .byte %01000000          ;; Dead tree        Not passable     See-through
     .byte %00000000          ;; Red wall         Not passable     Block sight
     .byte %00000000          ;; Red roof         Not passable     Block sight
     .byte %10100000          ;; Door             Passable         Block sight  Trigger
     .byte %11000000          ;; Road             Passable         See-through
     .byte %11000000          ;; Grass            Passable         See-through
     .byte %11000000          ;; Grass            Passable         See-through
     .byte %11000000          ;; Grass            Passable         See-through
     .byte %11000000          ;; Grass            Passable         See-through
     .byte %11000000          ;; Flowers          Passable         See-through
     .byte %11000000          ;; Flowers          Passable         See-through
     .byte %00000000          ;; Tree             Not passable     Block sight
     .byte %11000000          ;; Bridge           Passable         See-through
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %11000000
     .byte %11000000
     .byte %11000000
     .byte %11000000
     .byte %11000000
     .byte %11000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %00000000
     .byte %11000000
     .byte %11000000
     .byte %11000000
     .byte %11000000
     .byte %11000000
     .byte %11000000
     .byte %11000000
     .byte %11000000
     .byte %11000000
     .byte %11000000
     .byte %11000000
     .byte %11000000
     .byte %11000000
     .byte %11000000

;; +----------------------------------+
;; |                                  |
;; |    TILE SET DEFINITIONS          |
;; |                                  |
;; +----------------------------------+

; Outdoors tileset
outdoorsTileset:
     .byte $13 ; 19 tile definitions
     .byte $48, $48, $48, $48 ;; Nothing/Black   $00
     .byte $47, $20, $20, $47 ;; Rocks           $01
     .byte $46, $46, $46, $46 ;; Water           $02
     .byte $44, $45, $45, $44 ;; Road            $03
     .text "    "             ;; Background      $04
     .byte $43, $42, $40, $41 ;; Tree            $05
     .byte $4b, $4c, $49, $4a ;; Dead tree       $06
     .byte $4d, $4d, $4d, $4d ;; Red wall        $07
     .byte $4e, $4e, $4e, $4e ;; Red roof        $08
     .byte $51, $52, $4f, $50 ;; Door            $09
     .byte $45, $44, $44, $45 ;; RoadInverse     $0a
     .byte $20, $53, $54, $20 ;; Grass Half 1    $0b
     .byte $54, $53, $53, $54 ;; Grass Half 2    $0c
     .byte $54, $20, $20, $53 ;; Grass Half 1    $0d
     .byte $53, $54, $54, $53 ;; Grass Half 2    $0e
     .byte $20, $56, $20, $55 ;; Flowers Red     $0f
     .byte $56, $20, $55, $20 ;; Flowers Yellow  $10
     .byte $57, $58, $40, $41 ;; Tree Variant    $11
     .byte $59, $59, $59, $59 ;; Bridge          $12

outdoorsTilesetColorTable:
     .byte $00, $00, $00, $00 ;; Nothing / Black
     .byte $01, $01, $01, $01 ;; Rocks - Hires white
     .byte $1e, $1e, $1e, $1e ;; Water - blue
     .byte $1d, $1d, $1d, $1d ;; Road - green
     .byte $1d, $1d, $1d, $1d ;; Background - N/A
     .byte $1d, $1d, $1d, $1d ;; Tree - green
     .byte $1a, $1a, $1a, $1a ;; Dead tree - red
     .byte $1a, $1a, $1a, $1a ;; Red wall - red
     .byte $1a, $1a, $1a, $1a ;; Red roof - red
     .byte $08, $08, $08, $08 ;; Door
     .byte $1d, $1d, $1d, $1d ;; Road - green
     .byte $00, $00, $00, $00 ;; Grass
     .byte $00, $00, $00, $00 ;; Grass
     .byte $00, $00, $00, $00 ;; Grass
     .byte $00, $00, $00, $00 ;; Grass
     .byte $00, $02, $00, $00 ;; Flowers Yellow
     .byte $07, $00, $00, $00 ;; Flowers Red
     .byte $1d, $1d, $1d, $1d ;; Tree variant
     .byte $08, $08, $08, $08 ;; Bridge

outdoorsTilesetPropsTable:
     .byte %00000000          ;; Nothing / Black. Not passable.    Block Sight
     .byte %11000000          ;; Rocks.           Passable         See-through
     .byte %01000000          ;; Water            Not passable     See-through
     .byte %11000000          ;; Road             Passable         See-through
     .byte %11000000          ;; Background       Passable         See-through
     .byte %00000000          ;; Tree             Not passable     Block sight
     .byte %01000000          ;; Dead tree        Not passable     See-through
     .byte %00000000          ;; Red wall         Not passable     Block sight
     .byte %00000000          ;; Red roof         Not passable     Block sight
     .byte %10100000          ;; Door             Passable         Block sight  Trigger
     .byte %11000000          ;; Road             Passable         See-through
     .byte %11000000          ;; Grass            Passable         See-through
     .byte %11000000          ;; Grass            Passable         See-through
     .byte %11000000          ;; Grass            Passable         See-through
     .byte %11000000          ;; Grass            Passable         See-through
     .byte %11000000          ;; Flowers          Passable         See-through
     .byte %11000000          ;; Flowers          Passable         See-through
     .byte %00000000          ;; Tree             Not passable     Block sight
     .byte %11000000          ;; Bridge           Passable         See-through

;; Indoors tileset
indoorsTileset:
     .byte $11
     .byte $48, $48, $48, $48 ;; Nothing/Black      $00
     .byte $40, $40, $40, $40 ;; Wood floor         $01
     .byte $42, $42, $41, $41 ;; Wood wall w-e      $02
     .byte $48, $45, $48, $45 ;; Wood wall n-s(r)   $03
     .byte $45, $48, $45, $48 ;; Wood wall n-s(l)   $04
     .byte $48, $43, $48, $41 ;; Wood wall crn e-n  $05
     .byte $48, $46, $48, $45 ;; Wood wall crn e-s  $06
     .byte $47, $48, $45, $48 ;; Wood wall crn w-s  $07
     .byte $44, $48, $41, $48 ;; Wood wall crn w-n  $08
     .byte $55, $42, $53, $41 ;; Wood wall end w    $09
     .byte $42, $56, $41, $54 ;; Wood wall end e    $0a
     .byte $51, $52, $4f, $50 ;; Door               $0b
     .byte $49, $48, $45, $48 ;; Wood wall n-s-w    $0c
     .byte $59, $5a, $57, $58 ;; Table              $0d
     .byte $5b, $5c, $5d, $5e ;; Chair facing E     $0e
     .byte $60, $5f, $62, $61 ;; Chair facing W     $0f
     .byte $63, $64, $65, $66 ;; Stairs down        $10

indoorsTilesetColorTable;
     .byte $00, $00, $00, $00 ;; Nothing / Black
     .byte $1a, $1a, $1a, $1a ;; Wood floor
     .byte $08, $08, $08, $08 ;; Wood wall
     .byte $08, $08, $08, $08 ;; Wood wall
     .byte $08, $08, $08, $08 ;; Wood wall
     .byte $08, $08, $08, $08 ;; Wood wall corner
     .byte $08, $08, $08, $08 ;; Wood wall corner
     .byte $08, $08, $08, $08 ;; Wood wall corner
     .byte $08, $08, $08, $08 ;; Wood wall corner
     .byte $08, $08, $08, $08 ;; Wood wall end
     .byte $08, $08, $08, $08 ;; Wood wall end
     .byte $08, $08, $08, $08 ;; Door
     .byte $08, $08, $08, $08 ;; Wood wall n-s-w
     .byte $08, $08, $08, $08 ;; Table
     .byte $08, $08, $08, $08 ;; Chair facing E
     .byte $08, $08, $08, $08 ;; Chair facing W
     .byte $00, $00, $00, $00 ;; Chair facing W

indoorsTilesetPropsTable:
     .byte %00000000          ;; Nothing / Black. Not passable.    Block Sight
     .byte %11000000          ;; Wood floor       Passable         See-through
     .byte %00000000          ;; Wood wall        Not passable     Block Sight
     .byte %00000000          ;; Wood wall        Not passable     Block Sight
     .byte %00000000          ;; Wood wall        Not passable     Block Sight
     .byte %00000000          ;; Wood wall crn    Not passable     Block Sight
     .byte %00000000          ;; Wood wall crn    Not passable     Block Sight
     .byte %00000000          ;; Wood wall crn    Not passable     Block Sight
     .byte %00000000          ;; Wood wall crn    Not passable     Block Sight
     .byte %00000000          ;; Wood wall end    Not passable     Block Sight
     .byte %00000000          ;; Wood wall end    Not passable     Block Sight
     .byte %10100000          ;; Door             Passable         Block sight  Trigger
     .byte %00000000          ;; Wood wall T      Not passable     Block Sight
     .byte %01000000          ;; Wood Table       Not passable     See-through
     .byte %11000000          ;; Chair            Passable         See-through
     .byte %11000000          ;; Chair            Passable         See-through
     .byte %11100000          ;; Chair            Passable         See-through  Trigger


; Dungeon tile set
dungeonTileset:
     .byte $21
     .byte $48, $48, $48, $48       ;; Nothing/Black        $00
     .byte $40, $41, $41, $40       ;; Dungeon floor        $01
     .byte $41, $40, $40, $41       ;; Dungeon floor        $02
     .byte $42, $41, $42, $40       ;; Dungeon floor        $03
     .byte $44, $46, $43, $45       ;; Dungeon wall w-e     $04
     .byte $4d, $49, $4d, $47       ;; Dungeon wall n-s (l) $05
     .byte $4c, $4d, $4c, $4d       ;; Dungeon wall n-s (r) $06
     .byte $4d, $4a, $4d, $4f       ;; Dungeon wall s-e (l) $07
     .byte $50, $46, $51, $43       ;; Dungeon wall n-e (l) $08
     .byte $46, $53, $43, $52       ;; Dungeon wall n-w (r) $09
     .byte $54, $54, $4d, $4d       ;; Dungeon wall w-e (s) $0a
     .byte $4b, $54, $4c, $4d       ;; Dungeon wall s-e     $0b
     .byte $54, $55, $4d, $49       ;; Dungeon wall s-w     $0c
     .byte $4d, $4d, $4d, $4d       ;; Solid rock           $0d
     .byte $56, $4d, $57, $4d       ;; Dungeon wall s-w     $0e
     .byte $4b, $55, $4c, $49       ;; Dungeon wall s (end) $0f
     .byte $4c, $49, $4c, $47       ;; Dungeon wall n-s (l) $10
     .byte $59, $58, $51, $43       ;; Dungeon wall e       $11
     .byte $58, $5a, $45, $43       ;; Dungeon wall we      $12
     .byte $50, $53, $51, $52       ;; Dungeon wall s (end) $13
     .byte $4c, $4a, $4c, $4f       ;; Dungeon wall nse     $14
     .byte $5a, $5b, $45, $52       ;; Dungeon wall e (end) $15
     .byte $56, $4a, $57, $4f       ;; Dungeon wall wes     $16
     .byte $56, $54, $57, $4d       ;; Dungeon wall wes2    $17
     .byte $4b, $4a, $4c, $4f       ;; Dungeon wall n e     $18
     .byte $5e, $5f, $5c, $5d       ;; Stairs               $19
     .byte $62, $63, $60, $61       ;; Barrel               $1a
     .byte $64, $65, $66, $67       ;; Stairs down          $1b
     .byte $6a, $6b, $68, $69       ;; Stalactite           $1c
     .byte $56, $47, $57, $49       ;; Dungeon wall wsn     $1d
     .byte $54, $6c, $4d, $4f       ;; Dungeon wall ews,sw  $1e
     .byte $6d, $54, $57, $4d       ;; Dungeon wall ews,se  $1f
     .byte $6d, $55, $57, $49       ;; Dungeon wall ews,se  $20

dungeonTilesetColorTable:
     .byte $00, $00, $00, $00 ;; Nothing / Black
     .byte $08, $08, $08, $08 ;; Dungeon floor
     .byte $08, $08, $08, $08 ;; Dungeon floor
     .byte $08, $08, $08, $08 ;; Dungeon floor
     .byte $08, $08, $08, $08 ;; Dungeon wall w-e
     .byte $08, $08, $08, $08 ;; Dungeon wall n-s (l)
     .byte $08, $08, $08, $08 ;; Dungeon wall n-s (r)
     .byte $08, $08, $08, $08 ;; Dungeon wall s-e (r)
     .byte $08, $08, $08, $08 ;; Dungeon wall n-e
     .byte $08, $08, $08, $08 ;; Dungeon wall n-w
     .byte $08, $08, $08, $08 ;; Dungeon wall w-e (s)
     .byte $08, $08, $08, $08 ;; Dungeon wall s-e
     .byte $08, $08, $08, $08 ;; Dungeon wall s-w
     .byte $08, $08, $08, $08 ;; Solid rock
     .byte $08, $08, $08, $08 ;; Dungeon wall s-w
     .byte $08, $08, $08, $08 ;; Dungeon wall s (end)
     .byte $08, $08, $08, $08 ;; Dungeon wall n-s
     .byte $08, $08, $08, $08 ;; Dungeon wall e
     .byte $08, $08, $08, $08 ;; Dungeon wall w e
     .byte $08, $08, $08, $08 ;; Dungeon wall s (end)
     .byte $08, $08, $08, $08 ;; Dungeon wall nse
     .byte $08, $08, $08, $08 ;; Dungeon wall e (end)
     .byte $08, $08, $08, $08 ;; Dungeon wall wes
     .byte $08, $08, $08, $08 ;; Dungeon wall wes2
     .byte $08, $08, $08, $08 ;; Dungeon wall n e
     .byte $00, $00, $00, $00 ;; Stairs up
     .byte $1a, $1a, $1a, $1a ;; Barrel
     .byte $00, $00, $00, $00 ;; Stairs down
     .byte $08, $08, $08, $08 ;; Stalactite
     .byte $08, $08, $08, $08 ;; Dungeon wall wsn
     .byte $08, $08, $08, $08 ;; Dungeon wall ews,sw
     .byte $08, $08, $08, $08 ;; Dungeon wall ews,se
     .byte $08, $08, $08, $08 ;; Dungeon wall ws

dungeonTilesetPropsTable:
     .byte %00000000          ;; Nothing / Black. Not passable.    Block Sight
     .byte %11000000          ;; Dungeon floor    Passable         See-through
     .byte %11000000          ;; Dungeon floor    Passable         See-through
     .byte %11000000          ;; Dungeon floor    Passable         See-through
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %11100000          ;; Dungeon floor    Stairs up        See-through    Trigger
     .byte %01000000          ;; Barrel           Not passable.    See-through
     .byte %11100000          ;; Dungeon floor    Stairs down      See-through    Trigger
     .byte %00000000          ;; Stalactite       Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight
     .byte %00000000          ;; Dungeon wall     Not passable.    Block sight


.include "math.asm"


;; ----------------------
;; UTILITIES
;; ----------------------

print_source = $fb
print_source_length = $02
print_target = $fd
print_rowsize .byte $00
print_padding .byte $00
target_color .byte $00

print_string            ldy #$00
print_string_loop       lda (print_source), y
                        and #$3f
                        sta (print_target), y
                        iny
                        cpy print_source_length
                        bne print_string_loop
                        rts

print_string_right      lda print_rowsize
                        clc
                        sbc print_source_length
                        sta print_padding
                        ldy #$00
                        lda #$20
print_pad_loop          sta (print_target), y
                        iny
                        cpy print_padding
                        bne print_pad_loop
                        lda print_target
                        clc
                        adc print_padding
                        sta print_target
                        bcc no_pad_carry
                        inc print_target + 1
no_pad_carry            jmp print_string

pad_printed_string_to_num1
                        cpy num1
                        bcs pad_to_num1_done
                        sta (print_target), y
                        iny
                        jmp pad_printed_string_to_num1
pad_to_num1_done        rts

apply_text_color        ldy #$00
                        lda target_color
apply_text_color_loop   sta (print_target), y
                        iny
                        cpy print_source_length
                        bne apply_text_color_loop
                        rts

leftshift_2d            ldy #$00                    ; Rotate Left/Bit-shift with wrap
                        lda ($2d),y                 ; Operates on the value in zero-page adress $2d
                        asl
                        bcc leftshift_2d_nocarry
                        ora #$01
leftshift_2d_nocarry    sta ($2d),y
                        rts


rightshift_2d           ldy #$00                    ; Rotate Right/Bit-shift with wrap
                        lda ($2d),y                 ; Operates on the value in zero-page adress $2d
                        lsr
                        bcc rightshift_2d_nocarry
                        ora #$80
rightshift_2d_nocarry
                        sta ($2d),y
                        rts


clearscreen      ldx #$ff
clearscreenloop  inx
                 lda #$20     ; #$20 is the spacebar Screen Code
                 sta $0400,x
                 sta $0500,x
                 sta $0600,x
                 sta $06e8,x
                 lda #$01     ; set foreground to white in Color Ram
                 sta $d800,x
                 sta $d900,x
                 sta $da00,x
                 sta $dae8,x
                 cpx #$ff
                 bne clearscreenloop
                 rts

decBuffer       .byte $03
decBufferValue  .byte $00, $00, $00

byte_to_decimal:
                     lda #<decBufferValue
                     sta print_target
                     sta $20
                     lda #>decBufferValue
                     sta print_target+1
                     sta $21

                     jsr print_decimal
                     dec $20

                     ldx #$03
byte_to_decimal_loop lda decBufferValue
                     cmp #$30
                     bne byte_to_decimal_end
                     lda decBufferValue+1
                     sta decBufferValue
                     lda decBufferValue+2
                     sta decBufferValue+1
                     dex
                     cpx #$01
                     bne byte_to_decimal_loop
byte_to_decimal_end
                     stx decBuffer
                     rts

print_decimal:
         stx div_lo
         ldy #$00
         sty div_hi

         ldy #$02
nextdec  jsr divideby10
         ora #$30
         sta (print_target),y
         dey
         bpl nextdec
         rts

div_lo = $0800
div_hi = $0801

divideby10:
            ldx #$11
            lda #$00
            clc
div10loop   rol
            cmp #$0A
            bcc div10skip
            sbc #$0A
div10skip   rol div_lo
            rol div_hi
            dex
            bne div10loop
            rts

memcpy_rowSize .byte $00        ; Size of each row of mem to copy (in bytes)
memcpy_rows    .byte $00        ; Number of rows to copy

memcpy_readRowsByte:
            ldy #$00            ; Read number of rows from source mem area
            lda ($20), y
            sta memcpy_rows
            lda $20
            clc                 ; Move ptr to data
            adc #$01
            sta $20
            bcc memcpy
            inc $21

; Source ptr at $20-21, Target ptr at $22-23
memcpy:
               ldx #$00
memcpyrowloop  cpx memcpy_rows
               beq end_memcpy
               ldy #$00
memcpycolloop  lda ($20), y
               sta ($22), y
               iny
               cpy memcpy_rowSize
               bne memcpycolloop
               inx
incSrcPtr      lda $20
               clc
               adc memcpy_rowSize
               sta $20
               bcc incTrgPtr
               inc $21
incTrgPtr      lda $22
               clc
               adc memcpy_rowSize
               sta $22
               bcc memcpyrowloop
               inc $23
               jmp memcpyrowloop
end_memcpy     rts

memswitch_row:
               ldy #$00
memswitchloop  lda ($20), y
               tax
               lda ($22), y
               sta ($20), y
               txa
               sta ($22), y
               iny
               cpy memcpy_rowSize
               beq end_memcpy
               jmp memswitchloop

*=$0d20
.include "itemdb.asm"

*=$9000
.include "npcdb.asm"

;; +----------------------------------+
;; |                                  |
;; |    RESOURCE IMPORTS              |
;; |                                  |
;; +----------------------------------+

*=$2000
.binary "spritedata.raw"

*=$2800
.binary "dungeon-charset.bin"

*=$3000
.binary "indoors-charset.bin"

*=$3800
.binary "gamechars-charset.bin" 

; 15 bytes
