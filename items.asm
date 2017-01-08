;; +----------------------------------+
;; |                                  |
;; |    ITEM ROUTINES                 |
;; |                                  |
;; +----------------------------------+

getItemAt:
                    ldx #$00
                    cpx itemTableSize
                    beq endGetItemAt
                    jsr prepareItemIter
getItemAtLoop       lda ($20), y
                    and #%10000000
                    cmp #%10000000
                    bne getItemNextIter
                    iny
                    lda ($20), y
                    cmp tmpX
                    bne getItemNextIter
                    iny
                    lda ($20), y
                    cmp tmpY
                    bne getItemNextIter
                    txa
                    rts
getItemNextIter     inx
                    cpx itemTableSize
                    beq endGetItemAt
                    ldy #$00
                    jsr inc20Ptr
                    jmp getItemAtLoop
endGetItemAt        lda #$ff
                    rts

prepareItemIter:
                    lda #>itemTable
                    sta $21
                    lda #<itemTable
                    sta $20
                    lda itemTableRowSize
                    sta inc20ModVal
                    ldy #$00
                    rts

attemptPickUp:
                    lda playerX             ; Check for item at player Pos
                    sta tmpX
                    lda playerY
                    sta tmpY
                    jsr getItemAt
                    cmp #$ff
                    beq nothingToPickUp

                    ldy var_itemNamePtrLo   ; Store pointer to item name
                    lda ($20), y
                    sta tmpPtr1
                    iny
                    lda ($20), y
                    sta tmpPtr1+1

                    jsr addToInventory      ; Add item to Inventory

                    lda #<text_PICKED_UP
                    sta $20
                    lda #>text_PICKED_UP
                    sta $21
                    jsr addToMessageBuffer

                    ldx goldPickedUp
                    cpx #$00
                    beq pickUpNoAmount

                    jsr byte_to_decimal
                    jsr addToMessageBuffer
                    lda #$20
                    jsr addCharToMessageBuffer

pickUpNoAmount      lda tmpPtr1
                    sta $20
                    lda tmpPtr1+1
                    sta $21
                    jsr addToMessageBuffer
                    jsr addMessage

                    jmp dummyMove

nothingToPickUp     lda #<text_NOTHING_TO_PICK_UP
                    sta $20
                    lda #>text_NOTHING_TO_PICK_UP
                    sta $21
                    jsr addToMessageBuffer
                    jsr addMessage
                    rts

goldPickedUp .byte $00, $00

addToInventory:     ldy #$00
                    sty goldPickedUp
                    lda ($20), y
                    and #%01000000
                    cmp #%01000000
                    bne addToBackpack

                    lda ($20), y
                    and #%01111111
                    sta ($20), y

                    ldy var_itemValue
                    lda ($20), y
                    sta goldPickedUp
                    clc
                    adc playerGoldBalance
                    sta playerGoldBalance
                    jsr compactItemTable
                    rts

addToBackpack       lda #<backpackTable     ; Set pointer to backpack
                    sta $22
                    lda #>backpackTable
                    sta $23

                    lda backpackSize        ; Forward pointer to end of backpack
                    sta num1
                    lda backpackRowSize
                    sta num2
                    jsr multiply
                    sta inc22ModVal
                    jsr inc22Ptr

                    ldy #$00
                    lda ($20), y
                    sta ($22), y
                    and #%01111111
                    sta ($20), y

                    ldy var_itemTileID
                    lda ($20), y
                    ldy #$01
                    sta ($22), y

                    ldy var_itemNamePtrLo
                    lda ($20), y
                    ldy #$02
                    sta ($22), y

                    ldy var_itemNamePtrHi
                    lda ($20), y
                    ldy #$03
                    sta ($22), y

                    ldy var_itemValue
                    lda ($20), y
                    ldy #$04
                    sta ($22), y

                    inc backpackSize
                    jsr compactItemTable
                    rts
