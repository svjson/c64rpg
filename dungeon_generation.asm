

areaGen_items .byte $08
areaGen_feats .byte $25
areaGen_monsters .byte $08

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
                        ;lda #$13                 ; Set known seed for debug.
                        ;sta seed

                        ;ldx seed                ; Print seed as decimal for debug.
                        ;lda #<$0740
                        ;sta print_target
                        ;lda #>$0740
                        ;sta print_target+1
                        ;jsr print_decimal

                        ldx #$00                ; Clear loose ends from any previous run
                        lda #$ff
clearLooseEndsLoop      sta looseEndDir, x
                        inx
                        cpx #$08
                        bne clearLooseEndsLoop

                        lda #$48                ; Number of features/iterations
                        sta areaGen_feats
                        lda #$05                ; Number of items
                        sta areaGen_items
                        lda #$08
                        sta areaGen_monsters    ; Number of monsters

                        lda #$0d                ; Solid rock tile
                        sta brushTile           ; Fill entire buffer with rock
                        jsr fillLevel

                        lda #$01                ; Set brush tile to floor
                        sta brushTile

                        lda playerX             ; Start carving caves at player loc
                        sta brushX
                        sta entryX
                        lda playerY
                        sta brushY
                        sta entryY
                        jsr randomGenDir        ; Opposite direction of closest border, perhaps?
                        jsr addCaveRoom

genDungFeatsLoop
                        lda #$01                ; Set brush tile to floor
                        sta brushTile

                        lda #$0a                ; Randomly decide if to continue with current loose end
                        sta num3                ; or switch to another
                        jsr rndInt
                        cmp #$01
                        beq useRandomLE
                        jmp randomFeatType
useRandomLE             jsr getRandomActiveLE
randomFeatType          lda #$07                ; Get a random int, 0-7
                        sta num3
                        jsr rndInt
                        cmp #$01
                        bcc loopAddRoomFeat
                        jsr addCaveCorridor
                        jmp featAdded
loopAddRoomFeat         jsr addCaveRoom
featAdded
                        ldx leIndex             ; Disable loose end
                        lda #$ff
                        sta looseEndDir, x

                        dec areaGen_feats
                        lda areaGen_feats
                        cmp #$00
                        bne genDungFeatsLoop

generationDone          jsr addItemsToArea
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
caveCorrNoSolution      rts
addCaveCorridor         lda #$00
                        sta attempts
                        lda genDir
                        sta entryGenDir
addCaveCorrRetry        inc attempts
                        lda attempts
                        cmp #$05
                        beq caveCorrNoSolution

                        ldx leIndex
                        lda looseEndX, x
                        sta brushX
                        lda looseEndY, x
                        sta brushY

                        lda #$07            ; Get a random corridor length, 0-7 + 3
                        sta num3
                        jsr rndInt
                        adc #$03
                        sta corrLn
                        sta modVal
                        jsr isTargetOutOfBounds
                        cmp #$01
                        beq addCaveCorrRetry

                        ldx #$00
                        stx iter
addCaveCorridorLoop     cpx corrLn
                        beq addCaveCorridorDone
                        jsr brushCoordsToPtr
                        lda #$01
                        ldy #$00
                        sta ($22), y
                        jsr stepBrush

                        lda #$10          ; Get a random corridor length, 0-7 + 3
                        sta num3
                        jsr rndInt
                        cmp #$01
                        bne nextCorrLoopIter
                        jsr randomGenDirAheadOrTurn
                        jsr addLooseEnd
                        lda entryGenDir
                        sta genDir
nextCorrLoopIter        inc iter
                        ldx iter
                        jmp addCaveCorridorLoop

addCaveCorridorDone     jsr randomGenDirAheadOrTurn
                        jsr addLooseEnd
                        rts

;; +----------------------------------+
;; |    CAVE ROOM                     |
;; +----------------------------------+
entryGenDir .byte $00

addCaveRoom             lda genDir
                        sta entryGenDir
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
centered
                        jsr genSweepSteps
                        jmp checkSweepBounds

tryFewerSweepSteps      dec sweepSteps
checkSweepBounds        lda sweepSteps
                        cmp #$01
                        bmi endCaveSweep2
                        sta modVal
                        inc modVal
                        jsr isTargetOutOfBounds
                        cmp #$01
                        beq tryFewerSweepSteps

                        jsr genDirRight
                        lda #$02
                        sta modVal
                        jsr isTargetOutOfBounds
                        cmp #$01
                        bne prepCheckBrush

                        jsr genDir180
                        jsr stepBrush
                        jsr genDir180

prepCheckBrush          jsr genDirLeft
                        jmp checkBrushBounds

tryShorterBrush         dec sweepBrushLn
                        jmp checkBrushBoundsLoop
checkBrushBounds        jsr genDirLeft
checkBrushBoundsLoop    lda sweepBrushLn
                        cmp #$01
                        bmi endCaveSweep3
                        sta modVal
                        ;inc modVal
                        jsr isTargetOutOfBounds
                        cmp #$01
                        beq tryShorterBrush
                        jsr genDirRight
prepSweepLoop           ldx #$00                ; Set up iteration for sweeping
                        stx iter

sweepNStepsLoop         jsr brushCoordsToPtr
                        jsr storeBrushToPrevCoord
                        jsr applySweepBrush     ; Apply the brush
                        inc iter
                        ldx iter
                        cpx sweepSteps
                        beq endCaveSweep        ; End the sweeping if all steps have been performed

                        jsr returnBrushToPrevCoord
                        jsr stepBrush           ; ...and forward one step
                        jmp sweepNStepsLoop

endCaveSweep3           jsr genDirRight
                        jmp endCaveSweep2

endCaveSweep            ;jsr addLooseEnd
endCaveSweep2           inc genDir              ; Set up next sweep dir
                        lda genDir
                        cmp #$04
                        bne sweepNextDir

                        lda featOriginX         ; Return to origin of room for new dir
                        sta brushX
                        lda featOriginY
                        sta brushY

                        lda entryGenDir               ; Store loose end in dir of entry or turn
                        sta genDir
                        jsr randomGenDirAheadOrTurn
                        jsr addLooseEnd

                        jsr genDir180                 ; Store loose end in opposite dir or turn
                        jsr randomGenDirAheadOrTurn
                        jsr addLooseEnd
endAddCaveRoom          rts

sweepNextDir            jmp addCaveRoomDirLoop

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
                        stx sweepIter
                        jsr genSweepBLn
genSweepBrushLoop
                        lda #$01
                        sta num3
                        jsr rndInt
                        ldx sweepIter
                        cmp #$01
                        bne noSweepBrushIndent

                        lda sweepBrush
                        ora segMasks, x
                        sta sweepBrush

noSweepBrushIndent      inx
                        cpx sweepBrushLn
                        beq endSweepBrushGen
                        stx sweepIter
                        jmp genSweepBrushLoop
endSweepBrushGen        rts

genSweepSteps:          lda #$03
                        sta num3
                        jsr rndInt

                        sta sweepSteps
                        inc sweepSteps
                        inc sweepSteps
                        ldx genDir
                        rts

genSweepBLn:            lda #$03
                        sta num3
                        jsr rndInt
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

returnBrushToPrevCoord  lda prevCoordX          ; Return brush to previous location...
                        sta brushX
                        lda prevCoordY
                        sta brushY
                        rts

storeBrushToPrevCoord   lda brushX              ; Keep coords before modifying them
                        sta prevCoordX
                        lda brushY
                        sta prevCoordY
                        rts

randomGenDir:           lda #$03
                        sta num3
                        jsr rndInt
                        sta genDir
                        rts

randomGenDirAheadOrTurn:
                        lda #$03
                        sta num3
                        jsr rndInt
                        cmp #$02
                        bcc noTurn
                        cmp #$02
                        beq rGenDirTurnLeft
                        jsr genDirRight
                        rts
rGenDirTurnLeft         jsr genDirLeft
noTurn                  rts

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
                stx leIndex
                rts
findRandomActiveLE
                jsr getRandomActiveLE
                jmp foundLooseEnd

fraAttempt .byte $00
getRandomActiveLE
                lda #$00
                sta fraAttempt
getRandomActiveLERetry
                lda fraAttempt
                cmp #$0f
                beq createLooseEndFromRandomFloorSpot
                inc fraAttempt
                lda #$07
                sta num3
                jsr rndInt
                tax
                lda looseEndDir, x
                cmp #$ff
                beq getRandomActiveLERetry
                stx leIndex
                rts

createLooseEndFromRandomFloorSpot
                stx leIndex
                lda #$00
                sta fraAttempt

cLeRetry        jsr getRandomFloorTile
                cmp #$01
                bne cLeStoreLE

cLeNextAttempt  inc fraAttempt
                lda fraAttempt
                cmp #$30
                bne cLeRetry

                lda playerX
                sta brushX
                lda playerY
                sta brushY

cLeStoreLE      ldx leIndex
                lda brushX
                sta looseEndX, x
                lda brushY
                sta looseEndY, x

                jsr randomGenDir
                lda genDir
                ldx leIndex
                sta looseEndDir, x
                jmp getRandomActiveLE

getRandomFloorTileGrnt
                jsr getRandomFloorTile
                cmp #$01
                beq getRandomFloorTileGrnt
                rts

getRandomFloorTile:
                lda currentAreaWidth            ; Random X Pos
                sta num3
                dec num3
                jsr rndInt
                sta brushX

                lda currentAreaHeight           ; Random Y Pos
                sta num3
                dec num3
                jsr rndInt
                sta brushY

                ldx brushX                      ; Check bounds
                ldy brushY
                jsr isOutOfBounds
                cmp #$01
                beq getRDFTRetOOB

                jsr brushCoordsToPtr
                ldy #$00
                lda ($22), y
                cmp #$01
                bne getRDFTRetOOB
                lda #$00
                rts

getRDFTRetOOB   lda #$01
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
;; |    ITEM GENERATION               |
;; +----------------------------------+

totalRandWeight .byte $00
currentWeight   .byte $00

addItemsToArea:
                        ldx #$00
                        stx itemTableSize
                        lda #<itemTable
                        sta $20
                        lda #>itemTable
                        sta $21
                        lda itemTableRowSize
                        sta inc20ModVal

                        txa
                        clc
calcTotalRandWeight     adc itemSet_weight, x
                        inx
                        cpx itemSet_size
                        bne calcTotalRandWeight
                        sta totalRandWeight
                        dec totalRandWeight

addItemLoop
                        lda totalRandWeight
                        sta num3
                        jsr rndInt
                        sta currentWeight
                        ldx #$00
                        lda #$00
                        clc
findItemSetIndexLoop    adc itemSet_weight, x
                        cmp currentWeight
                        bcs itemSetIndexFound
                        inx
                        jmp findItemSetIndexLoop
itemSetIndexFound
                        lda itemSet_type, x         ; Look up the selected type
                        stx tmpX
                        tax

                        ldy var_itemModes           ; Copy from ItemDB to current items
                        lda itemAttributes, x
                        sta ($20), y
                        ldy var_itemTileID
                        lda itemTile, x
                        sta ($20), y
                        ldy var_itemNamePtrLo
                        lda itemNameLo, x
                        sta ($20), y
                        ldy var_itemNamePtrHi
                        lda itemNameHi, x
                        sta ($20), y
                        ldy var_itemValue
                        lda itemValue, x
                        sta ($20), y

                        jsr getRandomFloorTileGrnt  ; Select a random location
                        ldy var_itemXPos
                        lda brushX
                        sta ($20), y
                        ldy var_itemYPos
                        lda brushY
                        sta ($20), y

                        inc itemTableSize
                        jsr inc20Ptr

                        dec areaGen_items
                        ldx areaGen_items
                        cpx #$00
                        bne addItemLoop
                        rts

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

getRandomGroundTile     lda #$02
                        sta num3
                        jsr rndInt
                        clc
                        adc #$01
                        rts

findWallTile
                        jsr collectTileBits
                        lda tileBit
                        jsr getTileReplacement
                        rts

wallSpecs .byte $19

wallSpec
.byte %01011000                 ; 04
.byte %01010110                 ; 05
.byte %01001011                 ; 06
.byte %01011110                 ; 07
.byte %01001000                 ; 08
.byte %01010000                 ; 09
.byte %00011111                 ; 0a
.byte %00001011                 ; 0b
.byte %00010110                 ; 0c
.byte %01011011                 ; 0e
.byte %00000010                 ; 0f
.byte %01000010                 ; 10
.byte %00001000                 ; 11
.byte %00011000                 ; 12
.byte %01000000                 ; 13
.byte %01001010                 ; 14
.byte %00010000                 ; 15
.byte %00011010                 ; 16
.byte %11011011 ; Untested      ; 17
.byte %00001010                 ; 18
.byte %00000000
.byte %11010010
.byte %00011110
.byte %00011011
.byte %00010010

wallSpecInterestingBits
.byte %01011010                 ; 04
.byte %01011110                 ; 05
.byte %01011011                 ; 06
.byte %01011111                 ; 07
.byte %01011010                 ; 08
.byte %01011010                 ; 09
.byte %01011111                 ; 0a
.byte %01011011                 ; 0b
.byte %01011110                 ; 0c
.byte %01011111                 ; 0e
.byte %01011010                 ; 0f
.byte %01011010                 ; 10
.byte %01011010                 ; 11
.byte %01011010                 ; 12
.byte %01011010                 ; 13
.byte %01011011                 ; 14
.byte %01011010                 ; 15
.byte %00011111                 ; 16
.byte %11011111 ; Untested      ; 17
.byte %01011011                 ; 18
.byte %01011010
.byte %11011110
.byte %01011111
.byte %01011111
.byte %01011110


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
.byte $14
.byte $15
.byte $16
.byte $17   ; Untested
.byte $18
.byte $1c
.byte $1d
.byte $1e
.byte $1f
.byte $20


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

