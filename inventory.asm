;; +----------------------------------+
;; |    INVENTORY UI STATE            |
;; +----------------------------------+
invCrsrArea .byte $00           ; Currently Active Box - $00 = Backpack, $01 = Body, $02 = Floor

invSelArea  .byte $ff           ; Selection In Box - $00 = Backpack, $01 = Body, $02 = Floor, $ff = No selection
invSelPos   .byte $00           ; Position of selected item in box

invBPOffset .byte $00           ; Offset in Backpack
invFLOffset .byte $00           ; Offset in Floor

boxPositions:
invBPPos    .byte $00           ; Position in Backpack box
invBDPos    .byte $00           ; Position in Body
invFLPos    .byte $00           ; Position in Floor

floorTableOriginTable:
.byte $00
.byte $00
.byte $00
.byte $00
.byte $00
.byte $00
.byte $00

floorTableSize .byte $00
floorTable:
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00

;; +----------------------------------+
;; |    INITIALIZE INVENTORY MODE     |
;; +----------------------------------+
enterInventory:
                    lda #$00
                    sta invCrsrArea
                    sta invBPPos
                    sta invBPOffset
                    sta invFLOffset
                    sta invFLPos
                    sta invBDPos

                    lda #<inventoryirq    ; Disable game map interrupts
                    sta $0314
                    lda #>inventoryirq
                    sta $0315

                    lda #$00           ; Set screen background color
                    sta $d021
                    jsr clearscreen

                    lda #$82          ; Set cursor sprite pointers
                    sta $07f8
                    lda #$83          ; Set cursor sprite pointers
                    sta $07f9

                    lda #$03           ; Set cursor sprites to multicolor
                    sta $d01c

                    lda #$02           ; Set up sprite colors
                    sta $d027
                    sta $d028

                    lda #$0a
                    sta $d025

                    jsr populateFloorTable

                    jsr invPositionCrsr

                    lda #%00000011     ; Enable cursor sprites
                    sta $d015

                    lda #$00
                    sta boxTop
                    sta boxLeft
                    lda #$0f
                    sta boxWidth
                    lda #$13
                    sta boxHeight
                    jsr drawBox

                    lda #$10
                    sta boxLeft
                    lda #$17
                    sta boxWidth
                    jsr drawBox

                    lda #$01
                    sta memcpy_rowSize

                    lda #<text_BACKPACK
                    sta $20
                    lda #>text_BACKPACK
                    sta $21
                    lda #$12
                    sta $22
                    lda #$04
                    sta $23
                    jsr memcpy_readRowsByte

                    lda #$00
                    sta boxLeft
                    lda #$27
                    sta boxWidth
                    lda #$13
                    sta boxTop
                    lda #$06
                    sta boxHeight
                    jsr drawBox

                    lda #<text_FLOOR
                    sta $20
                    lda #>text_FLOOR
                    sta $21
                    lda #$fa
                    sta $22
                    lda #$06
                    sta $23
                    jsr memcpy_readRowsByte

                    jsr updateInventoryContents
                    jmp inventoryMainLoop


;; +----------------------------------+
;; |    INVENTORY MAIN LOOP           |
;; +----------------------------------+

key_INVENTORY_ACTION = #$20

inventoryMainLoop:
                    jsr inventoryReadKey

ilcont              lda #$15     ; wait for raster retrace
                    cmp $d012
                    bne ilcont

                    lda screenDirty
                    cmp #$00
                    beq inventoryMainLoop

                    jsr updateInventoryContents
                    lda #$00
                    sta screenDirty

                    jmp inventoryMainLoop

exitInventory:
                    jsr clearscreen
                    lda #<enterstatusirq    ; Interrupt vector
                    sta $0314
                    lda #>enterstatusirq
                    sta $0315
                    jsr enterMapMode
                    jsr initStatusArea
                    inc screenDirty
                    jmp mainloop

invPerformAction:
                    lda invSelArea
                    cmp #$ff
                    beq jmpSelectItem
                    jmp selectedItemAction
jmpSelectItem       jmp selectItem

inventoryReadKey:
                    jsr $ffe4
                    and #$3f

                    cmp key_INVENTORY
                    beq exitInventory

                    cmp key_INVENTORY_ACTION
                    beq invPerformAction

                    ldx invCrsrArea
                    cpx #$00
                    beq invReadBackpackAreaKey

                    cpx #$01
                    beq invReadBodyAreaKey

                    jmp invReadFloorAreaKey

invReadBackpackAreaKey:
                    cmp key_UP
                    beq moveBPCursorUp
                    cmp key_DOWN
                    beq moveBPCursorDown
                    cmp key_LEFT
                    beq moveBPCursorLeft
                    rts
moveBPCursorUp:
                    lda invBPPos
                    cmp #$00
                    beq moveInvNoAction
                    dec invBPPos
                    jmp invPositionCrsr
moveBPCursorDown:
                    ldx invBPPos
                    inx
                    cpx backpackSize
                    bcs invIntoFloorArea
                    inc invBPPos
                    jmp invPositionCrsr
moveBPCursorLeft:
                    jsr invIntoBodyArea
                    jmp invPositionCrsr
invReadBodyAreaKey:
                    cmp key_UP
                    beq moveBDCursorUp
                    cmp key_DOWN
                    beq moveBDCursorDown
                    cmp key_RIGHT
                    beq moveBDCursorRight
                    rts
moveBDCursorRight:
                    jsr invIntoBackPackArea
                    jmp invPositionCrsr
moveBDCursorUp:
                    rts
moveBDCursorDown:
                    jsr invIntoFloorArea
                    jmp invPositionCrsr
moveInvNoAction     rts

invReadFloorAreaKey:
                    cmp key_UP
                    beq moveFLCursorUp
                    cmp key_DOWN
                    beq moveFLCursorDown
                    rts
moveFLCursorUp:
                    ldx invFLPos
                    cpx #$00
                    beq invIntoBackPackArea
                    dec invFLPos
                    jmp invPositionCrsr

moveFLCursorDown:
                    ldx invFLPos
                    inx
                    cpx floorTableSize
                    bcs moveInvNoAction
                    inc invFLPos
                    jmp invPositionCrsr

invIntoBodyArea:
                    lda #$01
                    sta invCrsrArea
                    rts

invIntoBackPackArea:
                    lda #$00
                    sta invCrsrArea
                    jmp invPositionCrsr

invIntoFloorArea:
                    lda #$02
                    sta invCrsrArea
                    jmp invPositionCrsr

invPositionCrsr:
                    lda invCrsrArea
                    cmp #$00
                    beq invPositionBPCrsr

                    cmp #$01
                    beq invPositionBDCrsr

invPositionFLCrsr:
                    lda invFLPos
                    sta num1
                    lda #$10
                    sta num2
                    jsr multiply
                    clc
                    adc #$cf
                    sta $d001
                    sta $d003

                    lda #$1a
                    sta $d000
                    lda #$b0
                    sta $d002
                    lda #%00000000
                    sta $d010
                    rts
invPositionBDCrsr:
                    lda #$1a
                    sta $d000
                    lda #$80
                    sta $d002
                    lda #%00000000
                    sta $d010
                    rts
invPositionBPCrsr:
                    lda invBPPos
                    sta num1
                    lda #$10
                    sta num2
                    jsr multiply
                    clc
                    adc #$37
                    sta $d001
                    sta $d003

                    lda #$9a
                    sta $d000
                    lda #$30
                    sta $d002
                    lda #%00000010
                    sta $d010
                    rts

;; +----------------------------------+
;; |    INVENTORY ACTIONS             |
;; +----------------------------------+

selectItem:
        lda invCrsrArea
        sta invSelArea
        tax
        lda boxPositions, x
        sta invSelPos
        inc screenDirty
        rts
selectedItemAction:
        lda invCrsrArea                 ; branch to move item if origin and target area differ
        cmp invSelArea
        bne moveSelectedItem

        tax                             ; Trade places if positions differ
        lda boxPositions, x
        cmp invSelPos
        bne rearrangeItem

        lda #$ff                        ; Else, deselect
        sta invSelArea
        sta invSelPos
        inc screenDirty
        rts

moveSelectedItem
        lda invCrsrArea
        cmp #$02
        beq dropItem

        cmp #$00
        beq pickUpItem
        rts

rearrangeItem
        rts

targetPos .byte $00
pickUpItem:
        lda #<itemTable
        sta $20
        lda #>itemTable
        sta $21
        lda itemTableRowSize
        sta inc20ModVal

        ldx invFLPos
        lda floorTableOriginTable, x
        sta targetPos

        ldx #$00
pickUpForwardLoop:
        cpx targetPos
        beq pickUpForwarded
        inx
        jsr inc20Ptr
        jmp pickUpForwardLoop
pickUpForwarded:
        jsr addToInventory
        jsr populateFloorTable
        inc screenDirty
        lda #$ff
        sta invSelArea
        rts

dropItem:
                        lda #$ff
                        sta invSelArea

                        lda #<backpackTable
                        sta $20
                        lda #>backpackTable
                        sta $21
                        lda backpackRowSize
                        sta inc20ModVal

                        lda #<itemTable
                        sta $22
                        lda #>itemTable
                        sta $23
                        lda itemTableRowSize
                        sta inc22ModVal

                        ldy #$00
                        ldx #$00
forwardToItemLoop       cpx invSelPos
                        beq forwardedToItem
                        jsr inc20Ptr
                        inx
                        jmp forwardToItemLoop
forwardedToItem         ldx #$00
forwardToEndOfItems     cpx itemTableSize
                        beq forwardedToEnd
                        jsr inc22Ptr
                        inx
                        jmp forwardToEndOfItems

forwardedToEnd          ldy #$00
                        lda ($20), y
                        sta ($22), y
                        and #%01111111
                        sta ($20), y

                        iny
                        lda playerX
                        sta ($22), y
                        iny
                        lda playerY
                        sta ($22), y

                        ldy #$01
                        lda ($20), y
                        ldy #$03
                        sta ($22), y

                        ldy #$02
                        lda ($20), y
                        ldy #$04
                        sta ($22), y

                        ldy #$03
                        lda ($20), y
                        ldy #$05
                        sta ($22), y

                        ldy #$04
                        lda ($20), y
                        ldy #$06
                        sta ($22), y

                        inc itemTableSize
itemDropped
                        jsr compactBackpack
                        jsr populateFloorTable
                        inc screenDirty
                        rts

updateInventoryContents:
        jsr drawBackPack
        jsr drawFloor
        rts

;; +----------------------------------+
;; |    DRAW FLOOR CONTENTS           |
;; +----------------------------------+
drawFloor           ldx #$00

                    lda #$20
                    sta $20
                    sta $24
                    lda #$07
                    sta $21
                    lda #$db
                    sta $25

                    lda #<floorTable
                    sta $22
                    lda #>floorTable
                    sta $23
                    lda backpackRowSize
                    sta inc22ModVal

                    ldx #$00
                    stx iter
                    jmp drawFloorLoop
drawFloorDone       rts
drawFloorLoop       cpx floorTableSize
                    beq drawFloorDone

                    ldy #$01
                    lda ($22), y
                    tax                         ; Item Tile Index in X
                    ldy #$02                    ; Horiz position of backpack items
                    jsr drawBackpackItem

                    lda $20
                    clc
                    adc #$05
                    sta print_target
                    lda $21
                    sta print_target+1
                    ldy #$02
                    lda ($22), y
                    sta print_source
                    ldy #$03
                    lda ($22), y
                    sta print_source+1
                    ldy #$00
                    lda (print_source), y
                    sta print_source_length
                    inc print_source
                    jsr print_string

                    lda invSelArea              ; Check if area contains selection
                    cmp #$02
                    bne drawFLNoSelect

                    lda invSelPos               ; Check if current position is selected
                    cmp iter
                    bne drawFLNoSelect

                    lda #$03
                    sta target_color
                    jmp addFLNameColor

drawFLNoSelect      lda #$01
                    sta target_color
addFLNameColor      lda $24
                    clc
                    adc #$05
                    sta print_target
                    lda $25
                    sta print_target+1
                    jsr apply_text_color

                    jsr incscreenoffset

                    jsr inc22Ptr
                    inc iter
                    ldx iter
                    jmp drawFloorLoop

;; +----------------------------------+
;; |    DRAW BACKPACK CONTENTS        |
;; +----------------------------------+
drawBackPack        lda #$28
                    sta $20
                    sta $24
                    lda #$04
                    sta $21
                    lda #$d8
                    sta $25

                    lda #<backpackTable
                    sta $22
                    lda #>backpackTable
                    sta $23
                    lda backpackRowSize
                    sta inc22ModVal

                    ldx #$00
                    stx iter
                    jmp drawBackPackLoop
drawBackPackFillB   cpx #$08
                    beq drawBackPackDone
                    ldy #$12

fillBLoop           lda #$20
                    sta ($20), y
                    tya
                    clc
                    adc #$28
                    tay
                    lda #$20
                    sta ($20), y
                    tya
                    sbc #$27
                    tay
                    iny
                    cpy #$24
                    bne fillBLoop
                    jsr incscreenoffset
                    inx
                    jmp drawBackPackFillB
drawBackPackDone    rts
drawBackPackLoop    cpx backpackSize
                    beq drawBackPackFillB

                    lda $20

                    ldy #$01
                    lda ($22), y
                    tax                         ; Item Tile Index in X
                    ldy #$12                    ; Horiz position of backpack items
                    jsr drawBackpackItem

                    lda $20
                    clc
                    adc #$15
                    sta print_target
                    lda $21
                    sta print_target+1
                    ldy #$02
                    lda ($22), y
                    sta print_source
                    ldy #$03
                    lda ($22), y
                    sta print_source+1
                    ldy #$00
                    lda (print_source), y
                    sta print_source_length
                    inc print_source
                    jsr print_string

                    lda invSelArea              ; Check if area contains selection
                    cmp #$00
                    bne drawBPNoSelect

                    lda invSelPos               ; Check if current position is selected
                    cmp iter
                    bne drawBPNoSelect

                    lda #$03
                    sta target_color
                    jmp addBPNameColor

drawBPNoSelect      lda #$01
                    sta target_color
addBPNameColor      lda $24
                    clc
                    adc #$15
                    sta print_target
                    lda $25
                    sta print_target+1
                    jsr apply_text_color

                    jsr incscreenoffset

                    jsr inc22Ptr
                    inc iter
                    ldx iter
                    jmp drawBackPackLoop

drawBackpackItem
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

                    rts

;; +----------------------------------+
;; |    CURRENT POS INTERACTION       |
;; +----------------------------------+

populateFloorTable:
                    lda #$00
                    sta floorTableSize

                    lda #<itemTable
                    sta $20
                    lda #>itemTable
                    sta $21
                    lda itemTableRowSize
                    sta inc20ModVal

                    lda #<floorTable
                    sta $22
                    lda #>floorTable
                    sta $23
                    lda backpackRowSize
                    sta inc22ModVal

                    ldx #$00
floorTableLoop      cpx itemTableSize
                    beq endPopulateFloorTable

                    ldy var_itemXPos
                    lda ($20), y
                    cmp playerX
                    bne floorTableNextIter
                    ldy var_itemYPos
                    lda ($20), y
                    cmp playerY
                    bne floorTableNextIter

                    ldy #$00
                    lda ($20), y
                    sta ($22), y

                    ldy #$03
                    lda ($20), y
                    ldy #$01
                    sta ($22), y

                    ldy #$04
                    lda ($20), y
                    ldy #$02
                    sta ($22), y

                    ldy #$05
                    lda ($20), y
                    ldy #$03
                    sta ($22), y

                    ldy #$05
                    lda ($20), y
                    ldy #$03
                    sta ($22), y

                    txa
                    ldy floorTableSize
                    sta floorTableOriginTable, y

                    inc floorTableSize
                    jsr inc22Ptr

floorTableNextIter  jsr inc20Ptr
                    inx
                    jmp floorTableLoop

endPopulateFloorTable:
                    rts

;; +----------------------------------+
;; |    BACKPACK TABLE OPS            |
;; +----------------------------------+
tmpPos .byte $00
iterMax .byte $00

compactItemTable:
                    lda #<itemTable
                    sta $20
                    sta $22
                    lda #>itemTable
                    sta $21
                    sta $23

                    lda itemTableRowSize
                    sta inc20ModVal
                    sta inc22ModVal
                    sta memcpy_rowSize

                    lda #$01
                    sta memcpy_rows

                    lda itemTableSize
                    sta iterMax

                    ldx #$00
                    stx iter
compactITLoop       ldx iter
                    cpx iterMax
                    bcs endCompactIT

                    ldy #$00
                    lda ($20), y

                    and #%10000000
                    cmp #%10000000
                    beq compactITEntry
                    jsr inc20Ptr
                    dec itemTableSize
                    dec iterMax
compactITEntry
                    jsr memcpy              ; memcpy forwards both 20 and 22 pointers one step

                    inc iter
                    jmp compactITLoop

endCompactIT
                    rts

compactBackpack:
                    lda #<backpackTable         ; Set 20 and 22 pointers to backpack
                    sta $20
                    sta $22
                    lda #>backpackTable
                    sta $21
                    sta $23

                    lda backpackRowSize         ; Increment and copy full backpackTable row at a time
                    sta inc20ModVal
                    sta inc22ModVal
                    sta memcpy_rowSize

                    lda #$01                    ; Copy one row at a time
                    sta memcpy_rows

                    lda backpackSize
                    sta iterMax

                    ldx #$00
                    stx iter
compactBPLoop       ldx iter
                    cpx iterMax
                    bcs endCompactBP

                    ldy #$00
                    lda ($20), y

                    and #%10000000
                    cmp #%10000000
                    beq compactBPEntry
                    jsr inc20Ptr
                    dec backpackSize
                    dec iterMax
compactBPEntry
                    jsr memcpy              ; memcpy forwards both 20 and 22 pointers one step

                    inc iter
                    jmp compactBPLoop

endCompactBP
                    rts