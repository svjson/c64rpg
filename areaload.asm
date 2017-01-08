;; +----------------------------------+
;; |                                  |
;; |    AREA LOADING ROUTINES         |
;; |                                  |
;; +----------------------------------+

; Pointer to the currently loaded area
currentAreaPtr      .byte $00, $00

;; +----------------------------------+
;; |   LOAD AREA INTO CURRENT AREA    |
;; +----------------------------------+
; Input:    $20-$21       - Pointer of area to enter
;           trTargetIndex - Index of trigger in target area to enter at. #$ff if none.
;
; Not a subroutine. Exits with jmp.
enterArea:
                    lda currentAreaPtr+1    ; Store state of current area first, if we have one
                    cmp #$00
                    beq skipToLoad
                    jsr storeAreaState

skipToLoad          lda $20
                    sta currentAreaPtr
                    lda $21
                    sta currentAreaPtr+1
                    ldy #$00
                    lda ($20), y
                    sta currentAreaWidth
                    sta memcpy_rowSize    ; Also store as rowSize for memcpy
                    iny
                    lda ($20), y
                    sta currentAreaHeight
                    sta memcpy_rows       ; Also store as rows for memcpy to copy
                    iny
                    lda ($20), y
                    sta areaMode
                    iny
                    lda ($20), y
                    sta tilesetMask

                    iny
                    lda ($20), y           ; Set screen background color
                    sta sceneColBg
                    iny
                    lda ($20), y           ; Set character set color
                    sta sceneCol1
                    iny
                    lda ($20), y
                    sta sceneCol2

                    lda #$07              ; Skip ahead of already read map header
                    sta inc20ModVal
                    jsr inc20Ptr

                    lda areaMode          ; Generate level first if not visited before
                    and #%01000000
                    cmp #%01000000
                    bne loadMapData
                    jmp generateBeforeEnter

loadMapData         lda #<currentArea     ; Set currentArea map memory as target for memcpy
                    sta $22
                    lda #>currentArea
                    sta $23
                    jsr memcpy            ; Copy mapdata into currentArea

loadAreaTriggerTable lda triggerTableRowSize
                     sta memcpy_rowSize
                     lda #<triggerTable
                     sta $22
                     lda #>triggerTable
                     sta $23
                     jsr memcpy_readRowsByte
                     lda memcpy_rows
                     sta triggerTableSize   ; Trigger table size

loadAreaNpcTable      lda npcTableRowSize
                      sta memcpy_rowSize
                      lda #<npcTable
                      sta $22
                      lda #>npcTable
                      sta $23
                      jsr memcpy_readRowsByte
                      lda memcpy_rows
                      sta npcTableSize

loadAreaItemTable     lda itemTableRowSize
                      sta memcpy_rowSize
                      lda #<itemTable
                      sta $22
                      lda #>itemTable
                      sta $23
                      jsr memcpy_readRowsByte
                      lda memcpy_rows
                      sta itemTableSize

loadTileSetInfo       jsr doApplyTileSet 

doEnterArea:
                      lda trTargetIndex      ; This will be set to an index in the loaded trigger table
                      cmp #$ff               ; if we entered the area through a trigger exit of type index
                      beq doEnterCoords

                      jsr forwardTriggerTableToIndexA
                      ldy var_triggerXPos
                      lda ($22), y
                      sta playerX
                      iny
                      lda ($22), y
                      sta playerY

doEnterCoords         ldx playerX
                      stx tmpX
                      ldy playerY
                      sty tmpY

                      jmp performMove

;; +----------------------------------+
;; |   ENTER NOT-YET GENERATED AREA   |
;; +----------------------------------+
generateBeforeEnter:
                    ldy #$02                ; Skip past npc/item gen sets for now
                    lda ($20), y
                    sta areaGen_feats
                    iny
                    lda ($20), y
                    sta areaGen_npcs
                    iny
                    lda ($20), y
                    sta areaGen_items

                    lda #$05                ; Safe forward of ptr
                    sta inc20ModVal
                    jsr inc20Ptr

                    lda $20                 ; Store pointer to rest of level def,
                    sta triggerDefPtr       ; We'll want to keep reading later
                    lda $21
                    sta triggerDefPtr+1

                    jsr doApplyTileSet

                    jsr generateDungeon
                    jmp doEnterArea

;; +----------------------------------+
;; |   STORE STATE OF CURRENT AREA    |
;; +----------------------------------+
;                   Input
;                   Area addr  $20-21
storeAreaState:
                    lda $20       ; Store away new area to tmpPtr
                    sta tmpPtr1
                    lda $21
                    sta tmpPtr1+1

                    lda currentAreaPtr
                    sta $22
                    lda currentAreaPtr+1
                    sta $23

                    ldy #$02
                    lda areaMode
                    and #%10111111          ; Disable generation, in case we are storing a generated area
                    sta ($22), y

                    lda #$07                ; Skip to map data
                    sta inc22ModVal
                    jsr inc22Ptr
                    lda currentAreaWidth    ; Copy map data
                    sta memcpy_rowSize
                    lda currentAreaHeight
                    sta memcpy_rows

                    lda #<currentArea
                    sta $20
                    lda #>currentArea
                    sta $21
                    jsr memcpy

                    lda #$01                ; Further inc of $22-23 will be in increment of 1
                    sta inc22ModVal

                    ldy #$00
                    lda triggerTableSize
                    sta ($22), y
                    sta memcpy_rows
                    jsr inc22Ptr
                    lda triggerTableRowSize
                    sta memcpy_rowSize
                    lda #<triggerTable
                    sta $20
                    lda #>triggerTable
                    sta $21
                    jsr memcpy

                    ldy #$00
                    lda npcTableSize
                    sta ($22), y
                    sta memcpy_rows
                    jsr inc22Ptr
                    lda npcTableRowSize
                    sta memcpy_rowSize
                    lda #<npcTable
                    sta $20
                    lda #>npcTable
                    sta $21
                    jsr memcpy

                    ldy #$00
                    lda itemTableSize
                    sta ($22), y
                    sta memcpy_rows
                    jsr inc22Ptr
                    lda itemTableRowSize
                    sta memcpy_rowSize
                    lda #<itemTable
                    sta $20
                    lda #>itemTable
                    sta $21
                    jsr memcpy

                    lda tmpPtr1
                    sta $20
                    lda tmpPtr1+1
                    sta $21

                    rts

;; +----------------------------------+
;; |   APPLY LOADED TILE SET DATA     |
;; +----------------------------------+

doApplyTileSet:
selectTileSet         lda tilesetMask
                      cmp #%00001110
                      beq loadOutdoorsTiles
                      cmp #%00001010
                      beq loadDungeonTiles

loadIndoorsTiles      lda #<indoorsTileset
                      sta tmpPtr1
                      lda #>indoorsTileset
                      sta tmpPtr1+1
                      jmp loadTileset

loadOutdoorsTiles     lda #<outdoorsTileset
                      sta tmpPtr1
                      lda #>outdoorsTileset
                      sta tmpPtr1+1
                      jmp loadTileset

loadDungeonTiles      lda #<dungeonTileset
                      sta tmpPtr1
                      lda #>dungeonTileset
                      sta tmpPtr1+1
                      jmp loadTileset

loadTileset           lda #$01
                      lda tmpPtr1
                      sta $20
                      lda tmpPtr1+1
                      sta $21

                      ldy #$00            ; Read number of rows from source mem area
                      lda ($20), y
                      sta memcpy_rows     ; Store number of rows to read in memcpy_rows. Needed for tileProps
                      lda #$01
                      sta inc20ModVal
                      jsr inc20Ptr
                      lda #$04
                      sta inc20ModVal

                      ldx #$00
copyTileCharLoop      ldy #$00
                      lda ($20), y
                      sta tileChar1, x
                      iny
                      lda ($20), y
                      sta tileChar2, x
                      iny
                      lda ($20), y
                      sta tileChar3, x
                      iny
                      lda ($20), y
                      sta tileChar4, x
                      jsr inc20Ptr
                      inx
                      cpx memcpy_rows
                      bne copyTileCharLoop

                      ldx #$00
copyTileColorLoop     ldy #$00
                      lda ($20), y
                      sta tileCharColor1, x
                      iny
                      lda ($20), y
                      sta tileCharColor2, x
                      iny
                      lda ($20), y
                      sta tileCharColor3, x
                      iny
                      lda ($20), y
                      sta tileCharColor4, x
                      jsr inc20Ptr
                      inx
                      cpx memcpy_rows
                      bne copyTileColorLoop

copyTileProps
                      lda #<tileProps
                      sta $22
                      lda #>tileProps
                      sta $23
                      lda #$01
                      sta memcpy_rowSize
                      jsr memcpy

applyTileSetProps     lda $d018              ; Remap tileset
                      and #%11110001
                      ora tilesetMask
                      sta $d018

                      lda sceneCol1          ; Set character set color
                      sta $d022
                      lda sceneCol2
                      sta $d023
                      rts
