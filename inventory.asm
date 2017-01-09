;; +----------------------------------+
;; |    INVENTORY UI STATE            |
;; +----------------------------------+
invCrsrArea .byte $00           ; Currently Active Box - $00 = Backpack, $01 = Body, $02 = Floor
invCrsrAreaContentSize .byte $00

invSelArea  .byte $ff           ; Selection In Box - $00 = Backpack, $01 = Body, $02 = Floor, $ff = No selection
invSelPos   .byte $00           ; Position of selected item in box

boxOffsets:
invBPOffset .byte $00           ; Offset in Backpack
invBDOffset .byte $00
invFLOffset .byte $00           ; Offset in Floor

boxPositions:
invBPPos    .byte $00           ; Position in Backpack box
invBDPos    .byte $00           ; Position in Body
invFLPos    .byte $00           ; Position in Floor

boxSizes:
invBPSize   .byte $08
invBDSize   .byte $08
invFLSize   .byte $02

boxScrlUp:
invBPScrlUp .byte $00
invBDScrlUp .byte $00
invFLScrlUp .byte $00

boxScrlDn:
invBPScrlDn .byte $00
invBDScrlDn .byte $00
invFLScrlDn .byte $00

boxCrsrTopOffsets
            .byte $37
            .byte $37
            .byte $cf
            .byte $cf

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

backPackTop = $01
backPackLeft = $15

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
                    lda backpackSize
                    sta invCrsrAreaContentSize

                    lda #<inventoryirq    ; Disable game map interrupts
                    sta $0314
                    lda #>inventoryirq
                    sta $0315

                    lda #$00           ; Set screen background color
                    sta $d021
                    jsr clearscreen

                    lda $d018              ; Remap tileset
                    ora #%00001110
                    sta $d018
                    lda #$09               ; Set character set color
                    sta $d022
                    lda #$1d
                    sta $d023

                    lda #$82          ; Set cursor sprite pointers
                    sta $07f8
                    lda #$83          ; Set cursor sprite pointers
                    sta $07f9

                    lda #$84          ; Set nav arrow sprite pointers
                    sta $07fa
                    sta $07fc
                    lda #$85
                    sta $07fb
                    sta $07fd

                    lda #$03           ; Set cursor sprites to multicolor
                    sta $d01c

                    lda #$02           ; Set up sprite colors
                    sta $d027
                    sta $d028

                    lda #$0a
                    sta $d025
                    
                    lda #$0b           ; Set nav sprites to dark grey
                    sta $d029
                    sta $d02a
                    sta $d02b
                    sta $d02c

                    jsr populateFloorTable
                    jsr invPositionCrsr
                    
                    lda #$3d           ; Set arrows X POS
                    sta $d004
                    sta $d006
                    sta $d008
                    sta $d00a

                    lda #$38           ; Set BP nav sprite positions
                    sta $d005
                    lda #$af
                    sta $d007
                    
                    lda #$d0           ; Set FL nav sprite positions
                    sta $d009
                    lda #$e0
                    sta $d00b
                    
                    lda #%00111110     ; Set hi bits for bp nav sprites
                    sta $d010

                    lda #%00111111     ; Enable inventory sprites
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

                    lda #$13
                    sta boxTop
                    lda #$06
                    sta boxHeight
                    jsr drawBox

                    lda #$00
                    sta boxLeft
                    lda #$0f
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

                    lda #<text_FLOOR
                    sta $20
                    lda #>text_FLOOR
                    sta $21
                    lda #$0a
                    sta $22
                    lda #$07
                    sta $23
                    jsr memcpy_readRowsByte

                    lda #<text_PURSE
                    sta $20
                    lda #>text_PURSE
                    sta $21
                    lda #$fa
                    sta $22
                    lda #$06
                    sta $23
                    jsr memcpy_readRowsByte

                    lda #<text_GOLD
                    sta $20
                    lda #>text_GOLD
                    sta $21
                    lda #$4d
                    sta $22
                    lda #$07
                    sta $23
                    jsr memcpy_readRowsByte

                    lda #<$0749
                    sta $20
                    lda #>$0749
                    sta $21
                    lda #<$db49
                    sta $24
                    lda #>$db49
                    sta $25

                    ldx #$42                    ; Draw gold tile
                    clc
                    jsr drawItemTile

                    lda #$ff                    ; No selection on entry
                    sta invSelArea

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

                    jsr invPositionCrsr
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
                    inc screenDirty

                    cmp key_INVENTORY
                    beq exitInventory

                    cmp key_INVENTORY_ACTION
                    beq invPerformAction

                    ldx invCrsrArea
                    cpx #$01
                    beq invReadBodyAreaKey
                    jmp invReadItemContainerKey

invReadItemContainerKey
                    cmp key_UP
                    beq moveItemContCrsrUp
                    cmp key_DOWN
                    beq moveItemContCrsrDn
                    cmp key_LEFT
                    beq moveItemContCrsrLeft
                    dec screenDirty
                    rts

; CURSOR UP
moveItemContCrsrUp  ldx invCrsrArea
                    lda boxPositions, x
                    beq moveItemContUpTop
                    dec boxPositions, x
                    rts
moveItemContUpTop   lda boxOffsets, x
                    cmp #$00
                    beq leaveContUp
                    dec boxOffsets, x
                    rts

leaveContUp         cpx #$02
                    beq invIntoBackPackArea
                    rts

; CURSOR DOWN
moveItemContCrsrDn  ldx invCrsrArea
                    ldy boxPositions, x
                    iny
                    cpy invCrsrAreaContentSize
                    bcs leaveContDown
                    tya
                    cmp boxSizes, x
                    bcc incItemContCursor
                    lda #$01
                    cmp boxScrlDn, x
                    bne leaveContDown
                    inc boxOffsets, x
                    rts
incItemContCursor   inc boxPositions, x
                    rts

leaveContDown       cpx #$00
                    beq invIntoFloorArea
                    rts

; CURSOR LEFT
moveItemContCrsrLeft:
                    lda invCrsrArea
                    cmp #$00
                    bne moveInvNoAction
                    jmp invIntoBodyArea

; BODY AREA KEYS
invReadBodyAreaKey:
                    cmp key_UP
                    beq moveBDCursorUp
                    cmp key_DOWN
                    beq moveBDCursorDown
                    cmp key_RIGHT
                    beq moveBDCursorRight
                    rts

invIntoBackPackArea:
                    lda #$00
                    sta invCrsrArea
                    lda backpackSize
                    sta invCrsrAreaContentSize
                    jmp invPositionCrsr

invIntoFloorArea:
                    lda #$02
                    sta invCrsrArea
                    lda floorTableSize
                    sta invCrsrAreaContentSize
                    jmp invPositionCrsr

invIntoBodyArea:
                    lda #$01
                    sta invCrsrArea
;                    lda backpackSize
;                    sta invCrsrAreaContentSize
                    rts

moveBDCursorRight:
                    jmp invIntoBackPackArea
moveBDCursorUp:
                    rts
moveBDCursorDown:
                    jsr invIntoFloorArea
                    jmp invPositionCrsr
moveInvNoAction     rts

;; +----------------------------------+
;; |    CURSOR POSITIONING            |
;; +----------------------------------+
invPositionCrsr:
                    ldx invCrsrArea
                    lda boxPositions, x
                    sta num1
                    lda #$10
                    sta num2
                    jsr multiply
                    clc
                    ldx invCrsrArea                    
                    adc boxCrsrTopOffsets, x
                    
                    cpx #$01
                    beq invPositionBDCrsr
invPositionRightCol:
                    sta $d001
                    sta $d003
                    lda #$9a
                    sta $d000
                    lda #$30
                    sta $d002
                    lda #%00111110
                    sta $d010
                    rts
invPositionBDCrsr:
                    lda #$1a
                    sta $d000
                    lda #$80
                    sta $d002
                    lda #%00111100
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
        clc
        adc boxOffsets, x
        sta invSelPos
        inc screenDirty
        rts
selectedItemAction:
        lda invCrsrArea                 ; branch to move item if origin and target area differ
        cmp invSelArea
        bne moveSelectedItem

        tax                             ; Trade places if positions differ
        lda boxPositions, x
        clc
        adc boxOffsets, x
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
                        beq forwardToEndOfItems
                        jsr inc20Ptr
                        inx
                        jmp forwardToItemLoop
forwardToEndOfItems     lda itemTableSize
                        sta rollIterations
                        jsr roll22Ptr

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

                        lda invBPOffset             ; Adjust backpack offset if necessary
                        cmp #$00
                        beq itemDroppedNoAdjust
                        clc
                        adc #$08
                        cmp backpackSize
                        bcc itemDroppedNoAdjust
                        dec invBPOffset

itemDroppedNoAdjust     inc screenDirty
                        rts

updateInventoryContents:
                        jsr drawBackPack
                        lda invBPScrlUp
                        sta $d029
                        lda invBPScrlDn
                        sta $d02a
                        jsr drawFloor
                        lda invFLScrlUp
                        sta $d02b
                        lda invFLScrlDn
                        sta $d02c

                        ldx playerGoldBalance
                        jsr byte_to_decimal

                        lda #<decBuffer
                        sta $20
                        lda #>decBuffer
                        sta $21
                        lda #$75
                        sta $22
                        lda #$07
                        sta $23
                        lda #$01
                        sta memcpy_rowSize
                        jsr memcpy_readRowsByte

                        rts

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

                    lda #$00
                    sta itemContID
                    lda backpackSize
                    sta itemSourceSize

                    lda #$15
                    sta itemContTextOff
                    lda #$12
                    sta itemContTileOff
                    lda #$24
                    sta itemContRight

                    jmp drawItemContainer

;; +----------------------------------+
;; |    DRAW FLOOR CONTENTS           |
;; +----------------------------------+
drawFloor           lda #$20
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

                    lda #$02
                    sta itemContID
                    lda floorTableSize
                    sta itemSourceSize

                    lda #$15
                    sta itemContTextOff
                    lda #$12
                    sta itemContTileOff
                    lda #$24
                    sta itemContRight

                    jmp drawItemContainer

;; +----------------------------------+
;; |    DRAW ITEM CONTAINER           |
;; +----------------------------------+

itemContSize        .byte $00
itemSourceSize      .byte $00
itemContOffset      .byte $00
itemContTileOff     .byte $00
itemContTextOff     .byte $00
itemContRight       .byte $00
itemContID          .byte $00

drawItemContainer:
                    ldx itemContID

                    lda boxSizes, x     ; Look up source container size
                    sta itemContSize

                    lda boxOffsets, x   ; Roll item ptr to offset
                    sta rollIterations
                    sta itemContOffset
                    jsr roll22Ptr

                    ldx #$00
                    stx iter
                    jmp drawItemContLoop
itemContFillRemain  cpx itemContSize
                    bcs drawItemContDone
                    ldy itemContTileOff

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
                    cpy itemContRight
                    bne fillBLoop
                    jsr incscreenoffset
                    inx
                    jmp itemContFillRemain
drawItemContDone    jmp updateItemContainerArrows
                    rts
drawItemContLoop    cpx itemSourceSize
                    beq itemContFillRemain
                    cpx itemContSize
                    beq drawItemContDone

                    ldy #$01
                    lda ($22), y
                    tax                         ; Item Tile Index in X
                    ldy itemContTileOff         ; Horiz position of backpack items
                    jsr drawItemTile

                    lda $20
                    clc
                    adc itemContTextOff
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

                    lda #$20
drawItemBLTextLoop  cpy #$11
                    bcs drawItemContinue
                    sta (print_target), y
                    iny
                    jmp drawItemBLTextLoop

drawItemContinue
                    ldy invSelArea              ; Check if area contains selection
                    cpy itemContID
                    bne drawItemNoSelect

                    ldy invSelPos               ; Check if current position is selected
                    iny
                    tya
                    clc
                    sbc itemContOffset
                    cmp iter
                    bne drawItemNoSelect

                    lda #$03
                    sta target_color
                    jmp addItemNameColor

drawItemNoSelect    lda #$01
                    sta target_color
addItemNameColor    lda $24
                    clc
                    adc itemContTextOff
                    sta print_target
                    lda $25
                    sta print_target+1
                    jsr apply_text_color

                    jsr incscreenoffset

                    jsr inc22Ptr
                    inc iter
                    ldx iter
                    jmp drawItemContLoop

drawItemTile:
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

updateItemContainerArrows
                    ldy #$0b                ; Update upwards arrow
                    ldx itemContID
                    lda itemContOffset
                    cmp #$00
                    beq updateUpArrow
                    ldy #$01
updateUpArrow       tya
                    sta boxScrlUp, x

                    ldy #$0b                ; Update downwards arrow
                    ldx itemSourceSize
                    inx
                    cpx itemContSize
                    bcc updateDownArrow
                    lda itemContOffset
                    clc
                    adc itemContSize
                    cmp itemSourceSize
                    bcs updateDownArrow
                    ldy #$01
updateDownArrow     tya
                    ldx itemContID
                    sta boxScrlDn, x
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

;; +----------------------------------+
;; |    INVENTORY INTERRUPTS          |
;; +----------------------------------+
inventoryirq
                 lda #$18           ; Char multicolour mode on for entire screen
                 sta $d016

                 lda #<inventoryirq
                 sta $0314
                 lda #>inventoryirq
                 sta $0315

                 lda #$ff
                 sta $d012

                 asl $d019
                 jmp $ea31
