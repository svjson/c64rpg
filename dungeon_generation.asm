
feats .byte $25

brushX .byte $00
brushY .byte $00

prevCoordX .byte $00
prevCoordY .byte $00
entryX .byte $00
entryY .byte $00

featOriginX .byte $00
featOriginY .byte $00

brushPtr .byte $00, $00

floorTile .byte $01
brushTile .byte $00

sweepIter .byte $00
sweepSteps .byte $00

genDir .byte $00
genDirModX .byte $00, $01, $00, $ff
genDirModY .byte $ff, $00, $01, $00

sweepBrushLn .byte $00
sweepBrush .byte %00000000

corrLn .byte $00

bitMasks  .byte %10000000
          .byte %01000000
          .byte %00100000
          .byte %00010000
          .byte %00001000
          .byte %00000100
          .byte %00000010
          .byte %00000001

leIndex .byte $00

generateDungeon:
                        ldx seed
                        lda #<$0740
                        sta print_target
                        lda #>$0740
                        sta print_target+1
                        jsr print_decimal

                        lda #$25
                        sta feats

                        lda #$0d                ; Solid rock tile
                        sta brushTile           ; Fill entire buffer with rock
                        sta $d020
                        jsr fillLevel

                        lda #$01                ; Set brush tile to floor
                        sta brushTile

                        lda playerX             ; Start carving caves at player loc
                        sta brushX
                        sta entryX
                        lda playerY
                        sta brushY
                        sta entryY
                        jsr addCaveRoom

genDungFeatsLoop
                        lda #$01                ; Set brush tile to floor
                        sta brushTile
                        jsr getRandomActiveLE
                        jsr rndNum
                        sta num1
                        lda #$20
                        sta num2
                        lda #$07
                        sta num3
                        jsr divide_rndup
                        cmp #$01
                        bcc loopAddRoomFeat
;                        lda #$02
;                        sta $0722
                        jsr addCaveCorridor
                        jmp featAdded
loopAddRoomFeat         ;lda #$01
                        ;sta $0722
                        jsr addCaveRoom
featAdded
                        ldx leIndex
                        lda #$ff
                        sta looseEndDir, x
;                        lda #$03
;                        sta $0722
                        dec feats
                        lda feats
                        ;sta $0720
                        cmp #$00
                        bne genDungFeatsLoop

                        jsr decorateArea

                        lda entryX              ; Add stairs back up
                        sta brushX
                        lda entryY
                        sta brushY
                        jsr brushCoordsToPtr

                        lda #$19
                        ldy #$01
                        sta ($22), y

                        lda #$01
                        sta triggerTableSize
                        sta triggerTable+2
                        lda entryX
                        sta triggerTable
                        inc triggerTable
                        lda entryY
                        sta triggerTable+1
                        lda #<dungeoncellar
                        sta triggerTable+3
                        lda #>dungeoncellar
                        sta triggerTable+4
                        lda #$1e
                        sta triggerTable+5
                        lda #$14
                        sta triggerTable+6

                        rts

;; +----------------------------------+
;; |    CAVE CORRIDOR                 |
;; +----------------------------------+
addCaveCorridor         lda #$00
                        sta attempts
                        ;sta $0725

addCaveCorrRetry
                        ldx leIndex
                        lda looseEndX, x
                        sta brushX
                        lda looseEndY, x
                        sta brushY
                        jsr randomGenDir

                        jsr rndNum          ; Select a random dir
                        sta num1
                        lda #$20
                        sta num2
                        lda #$07
                        sta num3
                        jsr divide_rndup
                        adc #$03
                        sta corrLn
                        sta modVal

;                        lda #$01
;                        sta $0725

                        jsr isTargetOutOfBounds
                        cmp #$01
                        beq addCaveCorridor

;                        lda #$02
;                        sta $0725

                        ldx #$00
                        stx iter
addCaveCorridorLoop     cpx corrLn
                        beq addCaveCorridorDone
                        jsr brushCoordsToPtr
                        lda #$01
                        ldy #$00
                        sta ($22), y
                        jsr stepBrush
                        inc iter
                        ldx iter
                        jmp addCaveCorridorLoop

addCaveCorridorDone     jsr randomGenDir
                        jsr addLooseEnd
                        rts


;; +----------------------------------+
;; |    CAVE ROOM                     |
;; +----------------------------------+
addCaveRoom             lda #$02
                        sta $d020
                        lda #$00                ; Begin pushing upwards
                        sta genDir
                        lda brushX              ; Store current brush position as the origin of the cave room
                        sta featOriginX
                        lda brushY
                        sta featOriginY

addCaveRoomDirLoop      lda featOriginX         ; Return to origin of room for new dir
                        sta brushX
                        lda featOriginY
                        sta brushY
                        jsr generateSweepBrush  ; Generate a random sweep brush
                        jsr genDirRight         ; Center the brush around the origin
                        ldx #$00
                        ldy sweepBrushLn
                        dey
                        sty sweepSteps
centerSweepLoop         jsr stepBrush
                        cpx sweepSteps
                        inx
                        dec sweepSteps
                        bcs centerSweepLoop
                        jsr genDirLeft
centered                ldx #$00                ; Set up iteration for sweeping
                        stx iter
                        jsr genSweepSteps
sweepNStepsLoop         jsr brushCoordsToPtr
                        lda brushX              ; Keep coords before modifying them
                        sta prevCoordX
                        lda brushY
                        sta prevCoordY
                        jsr applySweepBrush     ; Apply the brush
                        inc iter
                        ldx iter
                        cpx sweepSteps
                        beq endCaveSweep        ; End the sweeping if all steps have been performed

                        lda prevCoordX          ; Return brush to previos location...
                        sta brushX
                        lda prevCoordY
                        sta brushY
                        jsr stepBrush           ; ...and forward one step
                        jmp sweepNStepsLoop

endCaveSweep            jsr addLooseEnd

                        inc genDir              ; Set up next sweep dir
                        lda genDir
                        cmp #$04
                        bne addCaveRoomDirLoop
                        dec genDir
endAddCaveRoom          rts

;; +----------------------------------+
;; |    SWEEP BRUSH ROUTINES          |
;; +----------------------------------+
applySweepBrush
                        ldx #$00
                        stx sweepIter
applySweepBrushLoop     lda brushTile
                        ldy #$00
                        sta ($22), y

                        lda sweepBrush
                        and segMasks, x
                        cmp segMasks, x
                        bne noSweepDent

                        jsr stepBrush
                        jsr brushCoordsToPtr

                        lda brushTile
                        ldy #$00
                        sta ($22), y

                        jsr genDir180
                        jsr stepBrush
                        jsr brushCoordsToPtr
                        jsr genDir180

noSweepDent             inc sweepIter
                        ldx sweepIter
                        cpx sweepBrushLn
                        beq sweepBrushApplied

                        jsr genDirLeft
                        jsr stepBrush
                        jsr brushCoordsToPtr
                        jsr genDirRight

                        jmp applySweepBrushLoop
sweepBrushApplied       rts

generateSweepBrush      ldx #$00
                        stx sweepBrush
                        jsr genSweepBLn
genSweepBrushLoop       jsr rndNum
                        sta num1
                        lda #$7f
                        sta num2
                        lda #$01
                        sta num3
                        jsr divide_rndup

                        cmp #$01
                        bne noSweepBrushIndent

                        lda sweepBrush
                        ora segMasks, x
                        sta sweepBrush

noSweepBrushIndent      inx
                        cpx sweepBrushLn
                        beq endSweepBrushGen
                        jmp genSweepBrushLoop
endSweepBrushGen        rts

genSweepSteps:          jsr rndNum
                        sta num1
                        lda #$40
                        sta num2
                        lda #$03
                        sta num3
                        jsr divide_rndup
                        sta sweepSteps
                        inc sweepSteps
                        inc sweepSteps
                        ldx genDir
                        rts

genSweepBLn:            jsr rndNum
                        sta num1
                        lda #$40
                        sta num2
                        lda #$03
                        sta num3
                        jsr divide_rndup
                        sta sweepBrushLn
                        inc sweepBrushLn
                        inc sweepBrushLn
                        ldx genDir
                        rts

;; +----------------------------------+
;; |    BRUSH ROUTINES MANIPULATION   |
;; +----------------------------------+
stepBrush               ldy genDir
                        lda brushX
                        clc
                        adc genDirModX, y
                        sta brushX
                        lda brushY
                        clc
                        adc genDirModY, y
                        sta brushY
                        rts

genDir180               jsr genDirLeft
                        jsr genDirLeft
                        rts

genDirLeft              dec genDir
                        lda genDir
                        cmp #$ff
                        bne genDirRts
                        lda #$03
                        sta genDir
genDirRts               rts

genDirRight             inc genDir
                        lda genDir
                        cmp #$04
                        bne genDirRts
                        lda #$00
                        sta genDir
                        rts

brushCoordsToPtr        lda #<currentArea
                        sta $22
                        lda #>currentArea
                        sta $23

                        lda currentAreaWidth
                        sta inc22ModVal
                        lda brushY
                        sta rollIterations
                        jsr roll22Ptr

                        lda brushX
                        sta inc22ModVal
                        jsr inc22Ptr

                        lda $22
                        sta brushPtr
                        lda $23
                        sta brushPtr+1
                        rts

randomGenDir:           jsr rndNum
                        sta num1
                        lda #$40
                        sta num2
                        lda #$03
                        sta num3
                        jsr divide_rndup
                        sta genDir
                        rts

;; +----------------------------------+
;; |    LOOSE ENDS                    |
;; +----------------------------------+

looseEndX   .byte $00, $00, $00, $00, $00, $00, $00, $00
looseEndY   .byte $00, $00, $00, $00, $00, $00, $00, $00
looseEndDir .byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff

addLooseEnd:
                ldx #$00
findFreeLE      lda looseEndDir, x
                cmp #$ff
                beq foundLooseEnd
                inx
                cpx #$08
                beq findRandomActiveLE
                jmp findFreeLE
foundLooseEnd:
                lda brushX
                sta looseEndX, x
                lda brushY
                sta looseEndY, x
                lda genDir
                sta looseEndDir, x
                rts
findRandomActiveLE
                jsr getRandomActiveLE
                jmp foundLooseEnd

getRandomActiveLE
                jsr rndNum          ; Select a random dir
                sta num1
                lda #32
                sta num2
                lda #$07
                sta num3
                jsr divide_rndup
                tax
                lda looseEndDir, x
                cmp #$ff
                beq getRandomActiveLE
                stx leIndex
                rts

;; +----------------------------------+
;; |    GENERAL AREA MANIPULATION     |
;; +----------------------------------+
fillLevel:
                        ldx #$00
                        stx iter
                        lda #<currentArea
                        sta $20
                        lda #>currentArea
                        sta $21
                        lda currentAreaWidth
                        sta inc20ModVal

fillLevelRow            ldy #$00
                        lda brushTile
fillLevelRowLoop        cpy currentAreaWidth
                        beq levelRowFilled
                        sta ($20), y
                        iny
                        jmp fillLevelRowLoop
levelRowFilled          inc iter
                        jsr inc20Ptr
                        ldx iter
                        cpx currentAreaHeight
                        bcs fillLevelComplete
                        jmp fillLevelRow
fillLevelComplete       rts

;; +----------------------------------+
;; |    AREA DECORATION               |
;; +----------------------------------+
rowIter .byte $00
colIter .byte $00

decorateArea:           lda #$05
                        sta $d020
                        lda #$00
                        sta brushX
                        sta brushY
                        sta rowIter
                        sta colIter
                        jsr brushCoordsToPtr
                        lda #$01
                        sta inc20ModVal
                        sta inc22ModVal

                        lda #<decorationBuffer
                        sta $20
                        lda #>decorationBuffer
                        sta $21

decorateColLoop         ldx colIter
                        ldy rowIter
                        ldy #$00
                        lda ($22), y
                        jsr getTileDecoration
                        ldy #$00
                        sta ($20), y
                        inc colIter
                        jsr inc20Ptr
                        jsr inc22Ptr
                        lda colIter
                        cmp currentAreaWidth
                        beq decorateNextRow
                        jmp decorateColLoop
decorateNextRow         inc rowIter
                        ldx #$00
                        stx colIter
                        lda rowIter
                        inc $d020
                        cmp currentAreaHeight
                        bne decorateColLoop

endDecorateArea         lda #$08
                        sta $d020
                        lda #<decorationBuffer          ; Copy decorated buffer back to level
                        sta $20
                        lda #>decorationBuffer
                        sta $21
                        lda #<currentArea
                        sta $22
                        lda #>currentArea
                        sta $23
                        lda currentAreaWidth
                        sta memcpy_rowSize
                        lda currentAreaHeight
                        sta memcpy_rows
                        jsr memcpy
                        lda #$00
                        sta $d020
                        rts

getTileDecoration       cmp #$01
                        beq getRandomGroundTile
                        jmp findWallTile
                        rts

getRandomGroundTile     jsr rndNum
                        sta num1
                        lda #$55
                        sta num2
                        lda #$02
                        sta num3
                        jsr divide_rndup
                        clc
                        adc #$01
                        rts

findWallTile
                        jsr collectTileBits
                        lda tileBit
                        jsr getTileReplacement
                        rts

wallSpecs .byte $15

wallSpec
.byte %01011000
.byte %01010010
.byte %01001010
.byte %01011010
.byte %01001000
.byte %01010000
.byte %00011110
.byte %00001010
.byte %00010010
.byte %01011010
.byte %00000010
.byte %01000010
.byte %00001000
.byte %00011000
.byte %01000000
.byte %01101010 ; Untested
.byte %00010000 ; Untested
.byte %11111010 ; Untested
.byte %11011011 ; Untested
.byte %00001011 ; Untested
.byte %00000000
.byte %11111111

wallSpecInterestingBits
.byte %01011010
.byte %01011010
.byte %01011010
.byte %01011011
.byte %01011010
.byte %01011010
.byte %01011110
.byte %01011010
.byte %01011010
.byte %01011110
.byte %01011010
.byte %01011010
.byte %01011010
.byte %01011010
.byte %01011010
.byte %01111011 ; Untested
.byte %01011010 ; Untested
.byte %11111111 ; Untested
.byte %11011111 ; Untested
.byte %01011011 ; Untested
.byte %01011010 ; Untested
.byte %11111111


wallSpecTargetTile
.byte $04
.byte $05
.byte $06
.byte $07
.byte $08
.byte $09
.byte $0a
.byte $0b
.byte $0c
.byte $0e
.byte $0f
.byte $10
.byte $11
.byte $12
.byte $13
.byte $14   ; Untested
.byte $15   ; Untested
.byte $16   ; Untested
.byte $17   ; Untested
.byte $18   ; Untested
.byte $1c


getTileReplacement      ldx #$00
                        stx tileBitIter
tileReplacementLoop     ldx tileBitIter
                        cpx wallSpecs
                        beq noTileReplacement

                        lda tileBit
                        and wallSpecInterestingBits, x
                        cmp wallSpec, x
                        beq foundTileReplacement
                        inc tileBitIter
                        jmp tileReplacementLoop
foundTileReplacement    lda wallSpecTargetTile, x
                        rts
noTileReplacement       lda #$0d
                        rts


tileBit .byte $00
tileBitIter .byte $00

collectTileBits         lda #$00
                        sta tileBit
                        sta tileBitIter
                        ldy rowIter
                        ldx colIter
                        dex
                        dey
                        stx tmpX
                        sty tmpY

collectTileBitsLoop     lda tileBitIter
                        jsr isOutOfBounds
                        cmp #$01
                        beq applyTileBit
                        jsr getTileAt
                        cmp #$0d
                        beq applyTileBit
                        jmp noTileBit
applyTileBit            ldx tileBitIter
                        lda bitMasks, x
                        ora tileBit
                        sta tileBit
noTileBit               lda tileBitIter
                        cmp #$02
                        beq nextTileBitRow
                        cmp #$04
                        beq nextTileBitRow
                        cmp #$07
                        beq tileBitCollected
                        inc tmpX
                        cmp #$03
                        beq skipTile
                        jmp nextTileBitIter
skipTile                inc tmpX
                        jmp nextTileBitIter
nextTileBitRow          dec tmpX
                        dec tmpX
                        inc tmpY
nextTileBitIter         inc tileBitIter
                        ldx tmpX
                        ldy tmpY
                        jmp collectTileBitsLoop
tileBitCollected        rts


;; +----------------------------------+
;; |    BOUNDS CHECK                  |
;; +----------------------------------+

isTargetOutOfBounds
                        lda brushX
                        sta tmpX
                        lda brushY
                        sta tmpY

isTargetOOBLoop         jsr stepBrush
                        ldx brushX
                        ldy brushY
                        cpx #$00
                        beq targetOutOfBoundsTrue
                        cpy #$00
                        beq targetOutOfBoundsTrue
                        jsr isOutOfBounds
                        cmp #$01
                        beq targetOutOfBoundsTrue
                        dec modVal
                        lda #$ff
                        cmp modVal
                        bne isTargetOOBLoop
                        lda #$00
                        jmp isTargetRestoreBrush
targetOutOfBoundsTrue   lda #$01
isTargetRestoreBrush    ldx tmpX
                        stx brushX
                        ldx tmpY
                        stx brushY
                        rts

isOutOfBounds
                        cpx currentAreaWidth
                        bcs outOfBoundsTrue
                        cpy currentAreaHeight
                        bcs outOfBoundsTrue
                        lda #$00
                        rts
outOfBoundsTrue         lda #$01
                        rts



decorationBuffer
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
     .byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00

