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
invBDSize   .byte $04
invFLSize   .byte $02

boxScrlUp:
invBPScrlUp .byte $00
invBDScrlUp .byte $00
invFLScrlUp .byte $00

boxScrlDn:
invBPScrlDn .byte $00
invBDScrlDn .byte $00
invFLScrlDn .byte $00

boxTableLo  .byte <(backpackTable)
            .byte <(bodyTable)
            .byte <(floorTable)

boxTableHi  .byte >(backpackTable)
            .byte >(bodyTable)
            .byte >(floorTable)

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

                    lda #<inventory_enterupper_irq    ; Disable game map interrupts
                    sta $0314
                    lda #>inventory_enterupper_irq
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

                    lda #$0c           ; Set body part sprites to gray
                    sta $d02d
                    sta $d02e

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
                    
                    lda #$20            ; Set body part sprite locations
                    sta $d00c
                    sta $d00e

                    lda #%00111110     ; Set hi bits for bp nav sprites
                    sta $d010

                    lda #%11111111     ; Enable inventory sprites
                    sta $d015

                    lda #$00
                    sta boxTop
                    sta boxLeft
                    lda #$10
                    sta boxWidth
                    lda #$13
                    sta boxHeight
                    jsr drawBox

                    lda #$11
                    sta boxLeft
                    lda #$16
                    sta boxWidth
                    jsr drawBox

                    lda #$13
                    sta boxTop
                    lda #$06
                    sta boxHeight
                    jsr drawBox

                    lda #$00
                    sta boxLeft
                    lda #$10
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
                    lda #%00000000     ; Disable all sprites
                    sta $d015
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

                    cmp key_UP
                    beq moveItemContCrsrUp
                    cmp key_DOWN
                    beq moveItemContCrsrDn
                    cmp key_LEFT
                    beq moveItemContCrsrLeft
                    cmp key_RIGHT
                    beq moveItemContCrsrRight
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

leaveContDown       cpx #$02
                    bcc invIntoFloorArea
                    rts

; CURSOR LEFT
moveItemContCrsrLeft:
                    lda invCrsrArea
                    cmp #$00
                    bne moveInvNoAction
                    jmp invIntoBodyArea

; CURSOR RIGHT
moveItemContCrsrRight
                    cpx #$01 
                    beq invIntoBackPackArea
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
                    lda #$04
                    sta invCrsrAreaContentSize
                    rts

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
                    sta $d001
                    sta $d003
                    lda #$12
                    sta $d000
                    lda #$90
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
rearrangeItem
        rts
moveSelectedItem
                        jsr set20PtrToSelectedItem                  ; Set container item pointers
                        jsr set22PtrToTargetItem

                        lda invCrsrArea                             ; Just move for non-floor targets
                        cmp #$02
                        bne mvItBetweenConts

                        jsr moveItemToFloor                         ; Go move to actual area item table if target is floor
                        jmp mvItPostCpy

mvItBetweenConts        ldy var_backpackItemModes                   ; Just copy to target, and set source item to off
                        lda ($20), y
                        sta ($22), y
                        and #%01111111
                        sta ($20), y
                        ldy var_backpackItemTileID
                        lda ($20), y
                        sta ($22), y
                        ldy var_backpackItemTypeID
                        lda ($20), y
                        sta ($22), y
                        ldy var_backpackItemIdentifyToTypeID
                        lda ($20), y
                        sta ($22), y
                        ldy var_backpackItemValue
                        lda ($20), y
                        sta ($22), y

                        lda invSelArea                              ; If source was floor, we'll have to go remove it from area item table
                        cmp #$02
                        bne mvItPostCpy
                        jsr removeFromFloor

mvItPostCpy             lda invCrsrArea                             ; If target was backpack, we'll have to increase the BP size
                        cmp #$00
                        beq mvItToBP
                        jmp endMvIt

mvItToBP                inc backpackSize
                        jmp endMvIt

endMvIt                 jsr compactBackpack                         ; Update all containers upon successful move
                        jsr compactItemTable
                        jsr populateFloorTable
                        lda #$ff
                        sta invSelArea
                        inc screenDirty
                        rts

moveItemToFloor         lda #<itemTable             ; Set Area Item Table as target
                        sta $22
                        lda #>itemTable
                        sta $23
                        lda itemTableRowSize
                        sta inc22ModVal
                        lda itemTableSize           ; Forward to end of table
                        sta rollIterations
                        jsr roll22Ptr

                        ldy var_itemModes
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

                        ldy var_backpackItemTileID
                        lda ($20), y
                        ldy var_itemTileID
                        sta ($22), y

                        ldy var_backpackItemTypeID
                        lda ($20), y
                        ldy var_itemTypeID
                        sta ($22), y

                        ldy var_backpackItemIdentifyToTypeID
                        lda ($20), y
                        ldy var_itemIdentifyToTypeID
                        sta ($22), y

                        ldy var_backpackItemValue
                        lda ($20), y
                        ldy var_itemValue
                        sta ($22), y

                        inc itemTableSize
                        rts

removeFromFloor         ldx boxPositions + 2
                        lda floorTableOriginTable, x
                        sta rollIterations
                        lda #<itemTable
                        sta $22
                        lda #>itemTable
                        sta $23
                        lda itemTableRowSize
                        sta inc22ModVal
                        jsr roll22Ptr
                        ldy var_itemModes
                        lda ($22), y
                        and #%01111111
                        sta ($22), y
                        rts

;; +----------------------------------+
;; |    LOCATE ITEMS IN TABLES        |
;; +----------------------------------+
set20PtrToSelectedItem  ldx invSelArea
                        lda boxTableLo, x
                        sta $20
                        lda boxTableHi, x
                        sta $21
                        lda backpackRowSize
                        sta inc20ModVal

                        ldx #$00
forwardToSelItemLoop    cpx invSelPos
                        beq forwardedToSelItem
                        jsr inc20Ptr
                        inx
                        jmp forwardToSelItemLoop
forwardedToSelItem      rts

set22PtrToTargetItem    ldx invCrsrArea
                        lda boxTableLo, x
                        sta $22
                        lda boxTableHi, x
                        sta $23
                        lda backpackRowSize
                        cpx #$00
                        beq forwardBPPos
                        cpx #$01
                        beq forwardBodyPos
forwardFLPos            lda floorTableSize
                        jmp doForwardToTargetPos
forwardBPPos            lda backpackSize
                        jmp doForwardToTargetPos
forwardBodyPos          sta inc22ModVal
                        lda boxPositions, x
doForwardToTargetPos    sta rollIterations
                        jsr roll22Ptr
                        rts

;; +----------------------------------+
;; |    UPDATE INVENTORY CONTENTS     |
;; +----------------------------------+
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
                        jsr drawBody

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
drawBackPack        lda #$3a
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

                    lda #$12
                    sta itemContTextWidth

                    jmp drawItemContainer

;; +----------------------------------+
;; |    DRAW FLOOR CONTENTS           |
;; +----------------------------------+
drawFloor           lda #$32
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

                    lda #$12
                    sta itemContTextWidth

                    jmp drawItemContainer

;; +----------------------------------+
;; |    DRAW BODY CONTENTS            |
;; +----------------------------------+
drawBody            lda #$29
                    sta $20
                    sta $24
                    lda #$04
                    sta $21
                    lda #$d8
                    sta $25

                    lda #<bodyTable
                    sta $22
                    lda #>bodyTable
                    sta $23
                    lda bodyTableRowSize
                    sta inc22ModVal
                    lda #$01
                    sta itemContID
                    lda #$04
                    sta itemSourceSize

                    lda #$0f
                    sta itemContTextWidth

                    jmp drawItemContainer

;; +----------------------------------+
;; |    DRAW ITEM CONTAINER           |
;; +----------------------------------+

itemContSize        .byte $00
itemSourceSize      .byte $00
itemContOffset      .byte $00
itemContTextWidth   .byte $00
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
itemContFillRemain  cpx itemContSize          ; Fill remaining rows in container with black
                    bcs drawItemContDone
                    ldy #$00

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
                    cpy itemContTextWidth
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

                    ldy var_backpackItemModes ; Check if there is an item here (always true for non-body)
                    lda ($22), y
                    and #%10000000
                    sta argItemModes
                    bne itmContActTile
                    ldx #$00
                    jmp itmContDrawTile

itmContActTile      ldy var_backpackItemTileID
                    lda ($22), y
                    tax                         ; Item Tile Index in X
itmContDrawTile     ldy #$00         ; Horiz position of container items
                    jsr drawItemTile

                    lda $20
                    clc
                    adc #$03
                    sta print_target
                    lda $21
                    sta print_target+1

                    lda argItemModes
                    cmp #%10000000
                    beq doPrintItemName

doPrintNothing      lda #<text_NOTHING
                    sta print_source
                    lda #>text_NOTHING
                    sta print_source+1
                    jmp doResolveNameLength
doPrintItemName     ldy var_backpackItemTypeID  ; Resolve item name from type
                    lda ($22), y
                    tax
                    lda itemNameLo, x
                    sta print_source
                    lda itemNameHi, x
                    sta print_source+1
doResolveNameLength ldy #$00
                    lda (print_source), y
                    sta print_source_length
                    lda itemContTextWidth       ; Check if we need to truncate
                    clc
                    sbc #$02
                    cmp print_source_length
                    bcs noItemNameTrunc
                    sta print_source_length
noItemNameTrunc     inc print_source
                    jsr print_string
                    lda itemContTextWidth
                    sbc #$03
                    sta num1
                    lda #$20
                    jsr pad_printed_string_to_num1

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
                    adc #$03
                    sta print_target
                    lda $25
                    sta print_target+1
                    jsr apply_text_color

                    jsr nextScreenRow

                    lda argItemModes
                    cmp #%10000000
                    beq drawItemSubLine
                    lda $20
                    sta print_target
                    lda $21
                    sta print_target+1
                    lda itemContTextWidth   ; Clear subline if no item present
                    sta num1
                    ldy #$00
                    lda #$20
                    jsr pad_printed_string_to_num1
                    jmp drawICNextIter

drawItemSubLine     lda $20                 ; Keep pointer
                    sta tmpPtr1
                    lda $21
                    sta tmpPtr1+1

                    ldy var_backpackItemValue
                    lda ($22), y
                    sta argItemVal
                    ldy var_backpackItemModes
                    lda ($22), y
                    sta argItemModes
                    jsr itemSubLineToPrintSource
                    ldy #$00
                    lda (print_source), y
                    sta print_source_length
                    inc print_source

                    lda tmpPtr1            ; Restore kept pointer
                    sta $20
                    clc
                    adc #$03
                    sta print_target
                    lda tmpPtr1+1
                    sta $21
                    sta print_target+1
                    lda itemContTextWidth
                    clc
                    sbc #$01
                    sta print_rowsize
                    jsr print_string_right

                    lda $24
                    clc
                    adc #$03
                    sta print_target
                    lda $25
                    sta print_target+1
                    lda itemContTextWidth
                    clc
                    sbc #$02
                    sta print_source_length
                    jsr apply_text_color
drawICNextIter      jsr nextScreenRow

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

nextScreenRow
                    lda $20
                    clc
                    adc #$28
                    bcc nSRNoCarry
                    inc $21
                    inc $25
nSRNoCarry
                    sta $20
                    sta $24
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

                    ldy var_itemModes
                    lda ($20), y
                    sta ($22), y

                    ldy var_itemTileID
                    lda ($20), y
                    ldy var_backpackItemTileID
                    sta ($22), y

                    ldy var_itemTypeID
                    lda ($20), y
                    ldy var_backpackItemTypeID
                    sta ($22), y

                    ldy var_itemIdentifyToTypeID
                    lda ($20), y
                    ldy var_backpackItemIdentifyToTypeID
                    sta ($22), y

                    ldy var_itemValue
                    lda ($20), y
                    ldy var_backpackItemValue
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

                    ldy var_backpackItemModes
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

placeholderMask .byte $00
;; +----------------------------------+
;; |    INVENTORY INTERRUPTS          |
;; +----------------------------------+
inventory_enterupper_irq
                 lda #$18           ; Char multicolour mode on for entire screen
                 sta $d016

                 lda #%00111111     ; Enable inventory sprites
                 sta placeholderMask

                 lda bodyTable
                 eor #%10000000
                 and #%10000000
                 lsr
                 ora placeholderMask
                 sta placeholderMask
                 lda bodyTable + 5
                 eor #%10000000
                 and #%10000000
                 ora placeholderMask
                 sta $d015

                 lda #$39           ; Set head and torso sprite positions
                 sta $d00d
                 lda #$49
                 sta $d00f

                 lda #$98           ; Set head and torso sprite pointers
                 sta $07fe
                 lda #$99
                 sta $07ff

                 lda #<inventory_leaveupper_irq
                 sta $0314
                 lda #>inventory_leaveupper_irq
                 sta $0315

                 lda #$58
                 sta $d012

                 asl $d019
                 jmp $ea31

inventory_leaveupper_irq

                 lda #$59
                 sta $d00d
                 lda #$69
                 sta $d00f

                 lda #%00111111     ; Enable inventory sprites
                 sta placeholderMask

                 lda bodyTable + 10
                 eor #%10000000
                 and #%10000000
                 lsr
                 ora placeholderMask
                 sta placeholderMask
                 lda bodyTable + 15
                 eor #%10000000
                 and #%10000000
                 ora placeholderMask
                 sta $d015

                 lda #$9b           ; Set right hand sprite pointer
                 sta $07ff

                 lda #<inventory_enterupper_irq
                 sta $0314
                 lda #>inventory_enterupper_irq
                 sta $0315

                 lda #$ff
                 sta $d012

                 lda #$9a           ; Set left hand sprite pointer
                 sta $07fe

                 asl $d019
                 jmp $ea81