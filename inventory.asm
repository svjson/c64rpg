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
drawBackPackLoop    cpx backpackSize
                    beq inventoryMainLoop

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

inventoryMainLoop:
                    jsr inventoryReadKey

ilcont              lda #$15     ; wait for raster retrace
                    cmp $d012
                    bne ilcont

                    lda screenDirty
                    cmp #$00
                    beq inventoryMainLoop

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

inventoryReadKey:
                    jsr $ffe4
                    and #$3f

                    cmp key_INVENTORY
                    beq exitInventory

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
                    jsr invIntoBackPackArea
                    jmp invPositionCrsr
moveFLCursorDown:
                    rts

invCrsrArea .byte $00           ; $00 = Backpack, $01 = Body, $02 = Floor
invBPOffset .byte $00
invBPPos    .byte $00
invFLOffset .byte $00
invFLPos    .byte $00
invBDPos    .byte $00

invIntoBodyArea:
                    lda #$01
                    sta invCrsrArea
                    rts

invIntoBackPackArea:
                    lda #$00
                    sta invCrsrArea
                    rts

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
                    lda #$a0
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
