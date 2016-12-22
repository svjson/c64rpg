*=$0801
.byte $0c, $08, $0a, $00, $9e, $20
.byte $34, $30, $39, $36, $00, $00
.byte $00

;; ------------
;; INIT ROUTINE
;; ------------
*=$1000

     jsr clearscreen

     lda #$00           ; Set border color
     sta $D020
     lda #$05           ; Set screen background color
     sta $D021
     
     lda #$09           ; Set character set color
     sta $D022
     lda #$1d
     sta $D023
          
     lda $d018          ; Remap character set
     ora #$0e
     sta $d018

     lda #$18           ; Char multicolour mode on 
     sta $d016    

     lda #$00
     sta $fb            ; raster counter
     
     lda #$81          ; Set player sprite pointers
     sta $07f8
     lda #$80          ; Set player sprite pointers
     sta $07f9
     
     lda #$03           ; Enable player sprites
     sta $d015
     lda #$10
     sta $d027
     
     lda #$08           ; Set up sprite colors
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
     
     ldx playerX
     ldy playerY
     jsr attemptMove

     jsr drawlevel

     jmp mainloop
     
     
text_HP     .text "HP:  012/014"    
text_EXP    .text "EXP: 050/100"

enterstatusirq   nop
                 nop
                 nop
                 nop
                 nop
                 nop
                 nop
                 lda #$00
                 sta $d021
                 
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

leavestatusirq   lda #$05           
                 sta $d021
                 
                 lda #%00011000 ; Enable multicolor text mode
                 sta $d016

                 lda #<enterstatusirq    ; Interrupt vector
                 sta $0314
                 lda #>enterstatusirq
                 sta $0315
                 
                 lda #$d2          ; Set raster line for next irq
                 sta $d012 

                 
                 asl $d019
                 jmp $ea81


;; ------------
;; MAIN LOOP
;; ------------
*=5000
mainloop            
                    jsr readKey

mlcont              lda #$15     ; wait for raster retrace
                    cmp $d012  
                    bne mlcont

                    lda screenDirty
                    cmp #$00
                    beq mainloop
                    
                    lda #$00
                    sta screenDirty
                    
                    jsr drawlevel
                    
                    jmp mainloop
    
;; -----------
;; KEY INPUT
;; -----------

key_UP    = #$17      ;; W,
key_LEFT  = #$01      ;; A
key_RIGHT = #$04      ;; D
key_DOWN   = #$18     ;; X

key_UPLEFT = #$11     ;; Q
key_UPRIGHT = #$05    ;; E
key_DOWNLEFT = #$1a   ;; Z
key_DOWNRIGHT = #$03  ;; C

readKey
                    ;jsr clearscreen
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

                    cmp #$06
                    beq toggleFOV

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

toggleFOV           lda areaMode
                    eor %10000000
                    sta areaMode
                    ldx playerX
                    ldy playerY
                    jmp attemptMove

movePerformed                    
                    inc screenDirty     
endReadKey                   
                    rts


attemptMove:        ; In parameters: target x, y in X and Y registers
                    jsr getTileAt       ; Look up tile at x, y
                    tax                 ; check if passable by comparing with tile table
                    lda iconprops, x
                    and #%10000000
                    cmp #%10000000
                    beq performMove
                    rts

performMove         ldx tmpX
                    ldy tmpY
                    stx playerX
                    sty playerY
                    inc screenDirty

                    clc
                    lda areaMode
                    and #%10000000
                    beq moveDone
                    ;jsr updateFOV           ; Deprecated soon-ish
                    jsr updateFOVLines
moveDone            rts

;; -----------
;; FOV ROUTINES
;; -----------

clearFOVBuffer:
                    lda #$10
                    ldx #$00
                    lda #$00
clearFOVLoop        sta fovBuffer, x
                    inx
                    cpx #$c8
                    bne clearFOVLoop
                    rts

; FOV segment pointers.
lSegmentAreaPtrHi
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $00
lSegmentAreaPtrLo
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $00

lSegmentFOVPtrHi
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $00
lSegmentFOVPtrLo
    .byte $00
    .byte $00
    .byte $00
    .byte $00
    .byte $00


currentLine .byte $00               ; Track current line of FOV sector being investigated
currentLineSegment .byte $00        ; Track the current segment of the line being investigated
lineLength  .byte $00               ; Length of the current line

fovSectorDir .byte $01              ; Positive or negative sector
fovHorVert  .byte $01               ; Horizontal or vertical sector
fovPtrModVal .byte $00
areaPtrModVal .byte $00

segMasks  .byte %10000000
          .byte %01000000
          .byte %00100000
          .byte %00010000
          .byte %00001000

updateFOVLines:
                    jsr clearFOVBuffer
                    ldx playerX     ; Put player coords X and Y
                    ldy playerY
                    jsr resolveTileRowPointer   ; Put player tile row pointer at $24-$25

                    clc
                    lda $24                     ; Forward pointer to actual player X pos
                    adc playerX
                    sta $24
                    bcc revealPlayerTile
                    inc $25

revealPlayerTile    ldy #$00                        ; Reveal tile player is standing on
                    lda ($24), y
                    sta fovBuffer+109

                    lda #$01
                    sta fovSectorDir                ; Negative/Positive
                    sta fovHorVert                  ; Horizontal/Vertical
walkFOVSector:
                    lda $25                         ; Store point of origin in first line segment pointer for area
                    sta lSegmentAreaPtrHi
                    lda $24
                    sta lSegmentAreaPtrLo

                    lda #>fovBuffer+109             ; Store point of origin in first line segment pointer for fov buffer
                    sta lSegmentFOVPtrHi
                    lda #<fovBuffer+109
                    sta lSegmentFOVPtrLo

                    lda #$13                        ; Set buffer widths as base mod factor
                    sta fovPtrModVal
                    lda currentAreaWidth
                    sta areaPtrModVal

                    ldx #$00
                    lda fovHorVert
                    cmp #$01
                    beq setStartVertical
setStartHorizontal  dec areaPtrModVal
                    lda fovSectorDir
                    cmp #$01
                    bne fovStartPtrLoop
                    dec fovPtrModVal                ; Buffer width+1 for horiz walks
                    dec areaPtrModVal
                    jmp fovStartPtrLoop
setStartVertical    inc fovPtrModVal                ; Buffer width-1 for vertical walks
                    lda fovSectorDir
                    cmp #$01
                    beq fovStartPtrLoop
                    inc fovPtrModVal
                    inc areaPtrModVal
fovStartPtrLoop     lda lSegmentFOVPtrLo, x         ; Mod FOV line segment ptr
                    sta incBuf
                    lda lSegmentFOVPtrHi, x
                    sta incBuf+1
                    lda fovPtrModVal
                    sta modVal
                    lda fovSectorDir
                    cmp #$01
                    beq fovStartPtrDec
fovStartPtrInc      jsr incPtr
                    jmp fovStartPtrSet
fovStartPtrDec      jsr decPtr
fovStartPtrSet      lda incBuf
                    sta lSegmentFOVPtrLo, x
                    lda incBuf+1
                    sta lSegmentFOVPtrHi, x

                    lda lSegmentAreaPtrLo, x         ; Mod area line segment ptr
                    sta incBuf
                    lda lSegmentAreaPtrHi, x
                    sta incBuf+1
                    lda areaPtrModVal
                    sta modVal
                    lda fovSectorDir
                    cmp #$01
                    beq areaStartPtrDec
areaStartPtrInc     jsr incPtr
                    jmp areaStartPtrSet
areaStartPtrDec     jsr decPtr
areaStartPtrSet     lda incBuf
                    sta lSegmentAreaPtrLo, x
                    lda incBuf+1
                    sta lSegmentAreaPtrHi, x

segPointersSet      txa
                    tay
                    inx
                    cpx #$05                       ; Move on if this was the last segment
                    bne prepareNextStartPtr
                    jmp beginLineWalks

prepareNextStartPtr                             ; Prepare next iteration by copying ptr positions to next round
                    lda lSegmentAreaPtrHi, y
                    sta lSegmentAreaPtrHi, x
                    lda lSegmentAreaPtrLo, y
                    sta lSegmentAreaPtrLo, x
                    lda lSegmentFOVPtrHi, y
                    sta lSegmentFOVPtrHi, x
                    lda lSegmentFOVPtrLo, y
                    sta lSegmentFOVPtrLo, x
                    jmp fovStartPtrLoop

beginLineWalks:
                    ldx #$00                        ; Set line index to 0 (of 8)
                    stx currentLine

walkFOVLine:        ;; Assume currentLine is loaded into X when we return here
                    lda fovLineTable, x
                    and #%00000111
                    sta lineLength                 ; Store away line length

                    ldx #$00
                    stx currentLineSegment
walkSegmentsLoop    lda lSegmentAreaPtrLo,x      ; Copy pointers to zero page
                    sta $22
                    lda lSegmentAreaPtrHi,x
                    sta $23

                    lda lSegmentFOVPtrLo,x
                    sta $20
                    lda lSegmentFOVPtrHi,x
                    sta $21

                    ldy #$00                   ; Copy tile from level data to fov buffer
                    lda ($22), y
                    sta ($20), y

                    tay
                    lda iconprops, y
                    and #%01000000
                    cmp #%01000000
                    bne lineWalked

                    inx
                    cpx lineLength
                    bne walkSegmentsLoop

lineWalked          inc currentLine
                    ldx currentLine
                    cpx #$0a                   ; 0a for all lines, There are ten lines to a sector
                    bne prepareNextLine

                    inc fovSectorDir
                    lda fovSectorDir
                    cmp #$03
                    beq nextOrientation
                    jmp walkFOVSector
nextOrientation     inc fovHorVert
                    lda fovHorVert
                    cmp #$03
                    beq endOfFov
                    lda #$01
                    sta fovSectorDir
                    jmp walkFOVSector
endOfFov
                    rts

prepareNextLine     lda fovHorVert              ; Prepare next FOV line
                    cmp #$01
                    beq prepHorLineStep
prepVertLineStep    lda currentAreaWidth
                    sta areaPtrModVal
                    lda #$14
                    sta fovPtrModVal
                    lda fovSectorDir
                    cmp #$01
                    beq prepareLoop
                    dec fovPtrModVal
                    dec areaPtrModVal
                    jmp prepareLoop
prepHorLineStep     lda #$01
                    sta areaPtrModVal
                    sta fovPtrModVal
                    lda fovSectorDir
                    cmp #$01
                    beq prepareLoop
                    dec fovPtrModVal
                    dec areaPtrModVal

prepareLoop         lda fovLineTable, x
                    ldx #$00
                    tay
modSegmentLoop      clc                 ; Set up segment modifiers for next round
                    and segMasks, x     ; Check if fovTable dictates seg mod
                    cmp segMasks, x
                    bne noSegMod
modFovPtr           lda fovPtrModVal
                    sta modVal
                    lda lSegmentFOVPtrLo, x
                    sta incBuf
                    lda lSegmentFOVPtrHi, x
                    sta incBuf+1
                    lda fovSectorDir
                    cmp #$01
                    bne decFovMod
incFovMod           jsr incPtr
                    jmp modAreaPtr
decFovMod           jsr decPtr
modAreaPtr          lda incBuf
                    sta lSegmentFOVPtrLo, x
                    lda incBuf+1
                    sta lSegmentFOVPtrHi, x

                    lda areaPtrModVal
                    sta modVal
                    lda lSegmentAreaPtrLo, x
                    sta incBuf
                    lda lSegmentAreaPtrHi, x
                    sta incBuf+1
                    lda fovSectorDir
                    cmp #$01
                    bne decAreaMod
incAreaMod          jsr incPtr
                    jmp storeAreaMod
decAreaMod          jsr decPtr
storeAreaMod        lda incBuf
                    sta lSegmentAreaPtrLo, x
                    lda incBuf+1
                    sta lSegmentAreaPtrHi, x
noSegMod
                    cpy #$00
                    beq prepareNextSeg
                    lda currentLine             ; This here is a little fix for the fifth segment
                    cmp #$03                    ; by running it an extra iteration through the loop
                    bcs prepareNextSeg
                    cpx #$04
                    bne prepareNextSeg
                    ldy #$00
                    jmp modFovPtr

prepareNextSeg      inx
                    tya
                    cpx #$05
                    bne contLoop
                    ldx currentLine
                    jmp walkFOVLine

contLoop
                    jmp modSegmentLoop


fovLineTable:
     ;;     MOD  LEN
     .byte %00001011  ; A-1     - 1
     .byte %00111100  ; A-2     - 2

     .byte %11111100  ; B-1     - 3
     .byte %01010101  ; B-2     - 4
     .byte %00100101  ; B-3     - 5
     .byte %00011101  ; B-4     - 6
     .byte %00011101  ; B-5     - 7
     .byte %00100101  ; B-6     - 8
     .byte %01010100  ; B-7     - 9

     .byte %11110100  ; C-1     - 10


;; ----------------------
;; LEVEL DRAWING ROUTINES
;; ----------------------

drawlevel;
                   lda #$04 ; Screen offset
                   sta $21
                   lda #$00
                   sta $20  

                   lda #$d8 ; Color RAM offset
                   sta $25
                   lda #$00
                   sta $24

                   lda areaMode             ; Jump to FOV mode setup of offsets and level data if FOV is on
                   and #%10000000
                   bne prepareFOVMode
                   sec                      ; Need to set carry after this operation. Not sure why
     
                   lda currentAreaHeight
                   sta drawBufferHeight
                   lda currentAreaWidth
                   sta drawBufferWidth

                   lda #>currentArea ; Level area offset in memory
                   sta $23
                   lda #<currentArea
                   sta $22
                   
                   lda playerY                    ; set up area offset relative to player coordinates
                   sbc #$05
                   sta currentAreaOffsetY

                   clc
                   lda playerX
                   sbc #$08
                   sta currentAreaOffsetX

                   lda currentAreaOffsetY         ; Set up counter for area row to be drawn
                   sta areaRow
     
                   ldx #$00                       ; Now offset pointer to the area data according to the screenOffsetY. 
dlOffsetLoop       cpx currentAreaOffsetY         ; Move level area offset pointers in $22, $23 according currentAreaOffsetY
                   beq dlOffsetCalculated         ; Nothing to do if Y is 0

                   lda currentAreaOffsetY         ; Now inc or dec offset counter until it matches the Y offset
                   cmp #$80
                   bcc dlOffsetPositive

dlOffsetNegative    dex
                    lda $22
                    sbc currentAreaWidth
                    sta $22
                    bcs dlOffsetLoop
                    dec $23
                    jmp dlOffsetLoop 
                    jmp dlOffsetCalculated


dlOffsetPositive    inx
                    lda $22
                    adc currentAreaWidth
                    sta $22
                    bcc dlOffsetLoop
                    inc $23
                    jmp dlOffsetLoop
                    jmp dlOffsetCalculated

prepareFOVMode
                   lda #>fovBuffer ; Level area offset in memory
                   sta $23
                   lda #<fovBuffer
                   sta $22

                   lda #$00
                   sta currentAreaOffsetX
                   sta currentAreaOffsetY
                   clc

                   lda #$14
                   sta drawBufferWidth
                   lda #$0a
                   sta drawBufferHeight
                   clc
                   lda #$00
                   sta areaRow

dlOffsetCalculated  lda #$00                   ; Set counters to zero
                    sta crsr
                    sta iter
                    sta drawat
          
                    clc
                    lda #$14                   ; Set lineEnd to current area X offset + screen width in tiles
                    adc currentAreaOffsetX     ; to indicate that we are one with a line when crsr has reached
                    sta lineEnd                ; that X position of level data
     
drawlevelloop
     inc iter
     ldx iter
     
     jsr drawline

     jsr incleveloffset
     jsr incscreenoffset
     inc areaRow

     ldx iter
     cpx #$0a
     bne drawlevelloop
     rts
     
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
     lda currentAreaOffsetX         ; Set up counter for area col to be drawn
     sta areaCol
     sta crsr
     lda #$00
     sta drawat

drawlineloop
     lda areaRow
     cmp drawBufferHeight
     bcs loademptytile     
     lda areaCol
     cmp drawBufferWidth
     bcs loademptytile     
     jmp loadtile
loademptytile     lda #$00
                  jmp loadtilea

loadtile    ldy crsr       ; Load source block index
            lda ($22), y

loadtilea   sta num1       ; Multiply by 4 to jump to source block data and put start pos in X
            lda #$04
            sta num2
            jsr multiply
            tax
 
drawtile     
     ldy drawat     ; Draw to screen     

     jsr drawchar
     iny
     inx
     jsr drawchar
     inx
     
     tya
     adc #$27
     tay
     
     jsr drawchar
     iny
     inx

     jsr drawchar     
     
     ldy drawat     
     iny
     iny
     sty drawat
     
     ldy crsr
     iny
     sty crsr
     
     inc areaCol
     
     cpy lineEnd
     bne drawlineloop

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

drawchar
     lda icons, x
     sta ($20), y
     lda iconcols, x         
     sta ($24), y
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

;; ----------------------
;; AREA FUNCTIONS
;; ----------------------

; Input: coords in X and Y. Return tile byte as A
getTileAt:
                    stx tmpX
                    sty tmpY

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
                    ldy tmpY
                    ldx tmpX
                    rts

; Input: coords in X and Y. Return tile byte as A
;getTileAt:
                    ;jsr resolveTileRowPointer
                    ldy tmpX
                    lda ($24), y
                    ldy tmpY
                    ldx tmpX
                    rts

;; Input: coords in X and Y
;; Return: Pointer to tile in level memory at $24, $25
resolveTileRowPointer:
                    stx tmpX
                    sty tmpY

                    ldy #$00
                    lda #>currentArea
                    sta $25
                    lda #<currentArea
                    sta $24

getTileYPtrLoop     cpy tmpY
                    bne contTilePtrLoop
                    rts
contTilePtrLoop     iny
                    lda $24
                    clc
                    adc currentAreaWidth
                    sta $24
                    bcc getTileYPtrLoop
                    inc $25
                    jmp getTileYPtrLoop

storeToFOVBuffer:
                    stx tmpX
                    sty tmpY

                    ldy #$00
                    lda #>fovBuffer
                    sta $25
                    lda #<fovBuffer
                    sta $24



;; ----------------------
;; STATUS AREA ROUTINES
;; ----------------------

initStatusArea  lda #$00
                sta $0720
                lda #$1c
                sta $0747
                lda #$1e
                sta $07e7
                lda #$1f
                sta $07c0

                lda #$1b
                ldx #$00
horlineloop     sta $0721, x
                sta $07c1, x
                inx
                cpx #$26
                bne horlineloop
                
                lda #$1d
vertlineloop    sta $0748        
                sta $076f        
                sta $0770        
                sta $0797
                sta $0798
                sta $07bf

                lda #$01
                ldx #$00                                
                
colloop         sta $db20, x
                inx
                cpx #$c8
                bne colloop
                
                lda #$0f
                sta $db62
                sta $db63

                lda #$0f
                sta $db8a
                sta $db8b
                sta $db8c
                
                sta $db6a
                sta $db92

                
                lda #<text_HP
                sta print_source
                lda #>text_HP
                sta print_source+1
                
                lda #<$0762
                sta print_target
                lda #>$0762
                sta print_target+1
                lda #$0c
                sta print_source_length
                jsr print_string

                lda #<text_EXP
                sta print_source
                lda #>text_EXP
                sta print_source+1
                
                lda #<$078a
                sta print_target
                lda #>$078a
                sta print_target+1
                lda #$0c
                sta print_source_length
                jsr print_string


                rts

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
    
;; ----------------------
;; UTILITIES
;; ----------------------

print_source = $fb
print_source_length = $02
print_target = $fd


print_string            ldy #$00
print_string_loop       lda ($fb), y
                        and #$3f
                        sta ($fd), y
                        iny      
                        cpy print_source_length
                        bne print_string_loop
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
    

    

clearscreen      lda #$20     ; #$20 is the spacebar Screen Code
                 sta $0400,x  
                 sta $0500,x 
                 sta $0600,x 
                 sta $06e8,x 
                 lda #$00     ; set foreground to black in Color Ram 
                 sta $d800,x  
                 sta $d900,x
                 sta $da00,x
                 sta $dae8,x
                 inx
                 bne clearscreen
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

;; ----------------------
;; SCENE STATE
;; ----------------------
currentAreaOffsetX  .byte $00
currentAreaOffsetY  .byte $04

playerX .byte $08
playerY .byte $06

screenDirty .byte $00

;; ----------------------
;; LEVEL DATA
;; ----------------------

*=$7000

currentAreaWidth = #$21
currentAreaHeight = #$17

drawBufferWidth = $0810
drawBufferHeight = $0811

areaMode:
    .byte %10000000    ; 0/1 = FOV mode on/off
                       ; Unused
                       ; Unused
                       ; Unused
                       ; Unused
                       ; Unused
                       ; Unused
                       ; Unused

currentArea
     .byte $05, $04, $03, $04, $05, $05, $04, $04, $04, $02, $02, $04, $0e, $0c, $05, $05, $05, $05, $04, $05, $05, $05, $05, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
     .byte $05, $05, $0a, $04, $04, $04, $05, $04, $0c, $02, $02, $0d, $04, $04, $04, $04, $04, $01, $05, $05, $04, $05, $04, $04, $04, $04, $04, $05, $04, $04, $04, $05, $0c
     .byte $04, $04, $03, $05, $04, $04, $05, $05, $04, $02, $02, $04, $0b, $04, $04, $04, $05, $05, $04, $05, $05, $05, $05, $04, $05, $04, $04, $04, $04, $04, $05, $05, $04
     .byte $05, $04, $0a, $04, $0b, $01, $05, $04, $0e, $02, $02, $02, $04, $0f, $04, $06, $04, $04, $04, $05, $05, $05, $05, $04, $04, $04, $04, $06, $05, $05, $06, $04, $04
     .byte $04, $04, $0a, $03, $0c, $04, $04, $04, $04, $04, $02, $02, $04, $04, $04, $06, $04, $04, $0d, $0c, $0b, $05, $04, $06, $05, $04, $04, $04, $0d, $0e, $05, $04, $04
     .byte $05, $04, $04, $0a, $05, $04, $04, $01, $04, $04, $02, $02, $04, $06, $05, $04, $04, $05, $04, $05, $04, $04, $05, $04, $04, $05, $04, $06, $06, $05, $04, $06, $04
     .byte $04, $04, $04, $03, $0a, $0d, $0e, $04, $0d, $02, $02, $0c, $04, $04, $05, $0e, $04, $04, $04, $05, $05, $05, $05, $04, $0d, $05, $05, $04, $04, $04, $04, $04, $0c
     .byte $04, $04, $04, $04, $03, $03, $05, $04, $04, $02, $02, $0b, $04, $05, $0b, $0c, $0d, $05, $04, $04, $05, $04, $04, $04, $0c, $04, $05, $04, $04, $04, $0e, $04, $04
     .byte $05, $04, $04, $04, $04, $0a, $03, $0a, $04, $02, $02, $04, $04, $04, $04, $0b, $04, $04, $0b, $04, $05, $05, $05, $04, $0b, $04, $04, $04, $04, $04, $0d, $05, $0b
     .byte $04, $04, $05, $04, $04, $04, $04, $0a, $03, $0a, $0a, $03, $0a, $03, $03, $04, $04, $04, $05, $05, $05, $04, $05, $04, $04, $04, $0b, $04, $0f, $04, $0c, $05, $04
     .byte $05, $05, $04, $04, $05, $04, $05, $04, $01, $02, $02, $04, $04, $04, $0a, $0a, $03, $0a, $03, $0a, $03, $0a, $03, $03, $04, $04, $0c, $0e, $04, $04, $04, $04, $05
     .byte $05, $04, $05, $04, $06, $04, $04, $0f, $05, $02, $02, $05, $05, $04, $05, $05, $04, $05, $04, $04, $05, $05, $04, $0a, $03, $04, $0d, $04, $04, $04, $04, $04, $05
     .byte $04, $0b, $05, $05, $04, $05, $05, $04, $05, $02, $02, $04, $05, $05, $05, $04, $04, $05, $05, $05, $05, $05, $05, $04, $0a, $04, $08, $08, $08, $08, $08, $08, $05
     .byte $04, $05, $04, $05, $05, $04, $05, $05, $04, $02, $02, $04, $0b, $05, $04, $04, $04, $04, $05, $04, $05, $04, $0b, $04, $03, $04, $08, $08, $08, $08, $08, $08, $04
     .byte $0d, $04, $04, $05, $04, $05, $05, $04, $02, $02, $04, $05, $0c, $0b, $05, $04, $04, $0f, $04, $05, $05, $04, $0d, $04, $0a, $04, $08, $08, $08, $08, $08, $08, $04
     .byte $04, $05, $05, $04, $05, $04, $04, $04, $02, $02, $0f, $0c, $0e, $04, $04, $04, $04, $05, $04, $05, $04, $05, $05, $04, $0a, $04, $08, $08, $08, $08, $08, $08, $04
     .byte $04, $04, $04, $05, $04, $04, $04, $02, $02, $02, $04, $0d, $04, $05, $04, $04, $04, $05, $05, $05, $04, $04, $05, $04, $03, $04, $07, $07, $07, $09, $07, $07, $04
     .byte $04, $04, $04, $04, $04, $02, $02, $02, $02, $04, $05, $04, $06, $05, $05, $04, $04, $04, $04, $04, $05, $05, $04, $04, $0a, $0a, $04, $06, $04, $0a, $04, $05, $04 
     .byte $0b, $04, $04, $04, $02, $02, $02, $02, $04, $04, $04, $04, $06, $06, $05, $05, $04, $04, $05, $05, $04, $04, $04, $04, $04, $03, $03, $0a, $0a, $03, $03, $0a, $03 
     .byte $04, $05, $04, $04, $02, $02, $04, $04, $04, $0b, $04, $04, $04, $04, $05, $05, $01, $04, $05, $04, $05, $04, $05, $04, $04, $04, $04, $04, $0f, $04, $04, $04, $04
     .byte $0d, $04, $04, $02, $02, $04, $04, $0d, $0e, $0c, $05, $05, $05, $04, $05, $0b, $0f, $10, $04, $04, $05, $04, $05, $04, $04, $0b, $0e, $0d, $05, $06, $04, $0f, $04 
     .byte $04, $05, $01, $02, $02, $04, $04, $04, $04, $0d, $04, $04, $04, $05, $04, $0e, $0c, $04, $05, $01, $04, $04, $04, $04, $0b, $0c, $0c, $0e, $04, $05, $04, $04, $04 
     .byte $04, $04, $04, $02, $02, $04, $04, $04, $04, $04, $04, $04, $05, $04, $04, $04, $0d, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04 

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

;; ----------------------
;; TILE DATA
;; ----------------------

*=$3000
icons:
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
     
iconcols:
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
     
iconprops:
     .byte %00000000          ;; Nothing / Black. Not passable.    Block Sight
     .byte %11000000          ;; Rocks.           Passable         See-through
     .byte %01000000          ;; Water            Not passable     See-through
     .byte %11000000          ;; Road             Passable         See-through
     .byte %11000000          ;; Background       Passable         See-through
     .byte %00000000          ;; Tree             Not passable     Block sight
     .byte %01000000          ;; Dead tree        Not passable     See-through
     .byte %00000000          ;; Red wall         Not passable     Block sight
     .byte %00000000          ;; Red roof         Not passable     Block sight
     .byte %10000000          ;; Door             Passable         Block sight
     .byte %11000000          ;; Road             Passable         See-through
     .byte %11000000          ;; Grass            Passable         See-through
     .byte %11000000          ;; Grass            Passable         See-through
     .byte %11000000          ;; Grass            Passable         See-through
     .byte %11000000          ;; Grass            Passable         See-through
     .byte %11000000          ;; Flowers          Passable         See-through
     .byte %11000000          ;; Flowers          Passable         See-through

;; ----------------------
;; MATH
;; ----------------------

num1        .byte $00                ; Math input #1
num2        .byte $00                ; Math input #2

;--------

multiply         lda #$00            ; Multiply with carry 
                 beq mply_enterLoop

mply_doAdd       clc
                 adc num1

mply_loop        asl num1
mply_enterLoop   lsr num2
                 bcs mply_doAdd
                 bne mply_loop
                 rts

;--------

modVal .byte $00
incBuf .byte $00
       .byte $00

incPtr:             ; Helper subroutine for increasing 16-bit buffer value
                    lda incBuf
                    clc
                    adc modVal
                    sta incBuf
                    bcs incCarry
                    rts
incCarry            inc incBuf+1
                    rts

decPtr              ; Helper subroutine for decreasing 16-bit buffer value
                    lda incBuf
                    clc
                    sbc modVal
                    sta incBuf
                    bcc decCarry
                    rts
decCarry            dec incBuf+1
                    rts


*=$2000
.binary "spritedata.raw"

*=$3800
.binary "gamechars-charset.bin" 

; 15 bytes