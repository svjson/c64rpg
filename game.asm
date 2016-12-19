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
     
     lda #$64           ; Set player sprite coords
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
     
     ; sei                 ;enable interrupts

     jsr drawlevel

     jmp mainloop
     
     
text_HP     .text "HP:  012/014"    
text_EXP    .text "EXP: 050/100"

enterstatusirq   lda #$00
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

key_UP    = #$17    ;; W,
key_LEFT  = #$01    ;; A

key_RIGHT = #$04    ;; D,
key_DOWN   = #$18   ;; X

readKey
                    ;jsr clearscreen
                    jsr $ffe4
                    and #$3f

                    cmp key_UP
                    beq move_up                    

                    cmp key_LEFT
                    beq move_left

                    cmp key_DOWN
                    beq move_down                  
                  
                    cmp key_RIGHT
                    beq move_right

                    jmp endReadKey
                    
move_up             ;lda $d001
                    ;sbc #$10
                    ;sta $d001                  
                    ;sta $d003
                    dec currentAreaOffsetY
                    jmp movePerformed                   
                    
move_left           ;lda $d000
                    ;sbc #$10      
                    ;sta $d000
                    ;sta $d002
                    dec currentAreaOffsetX
                    jmp movePerformed                   
                    
move_down           ;lda $d001
                    ;adc #$0f
                    ;sta $d001                  
                    ;sta $d003
                    inc currentAreaOffsetY
                    jmp movePerformed                   
                    
move_right          ;lda $d000      
                    ;adc #$0f  
                    ;sta $d000
                    ;sta $d002
                    inc currentAreaOffsetX
                    jmp movePerformed                   
                    
movePerformed                    
                    inc screenDirty     
endReadKey                   
                    rts



;; ----------------------
;; LEVEL DRAWING ROUTINES
;; ----------------------

drawlevel          
     lda #$04 ; Screen offset
     sta $21
     lda #$00
     sta $20  

     lda #$d8 ; Color RAM offset
     sta $25
     lda #$00
     sta $24
     
     lda #>currentArea ; Level area offset in memory
     sta $23
     lda #<currentArea
     sta $22
     
     lda currentAreaOffsetY         ; Set up counter for area row to be drawn
     sta areaRow
     
     ldy #$00         
dlOffsetLoop
     cpy currentAreaOffsetY         ; Move level area offset pointers in $22, $23 according 
     beq dlOffsetContinue           ; currentAreaOffsetY
     iny
     lda $22
     adc currentAreaWidth
     sta $22
     bcc dlOffsetLoop
     inc $23
     jmp dlOffsetLoop 
     
dlOffsetContinue
     lda #$00                   ; Set counters to zero
     sta crsr
     sta iter
     sta drawat
     
     lda #$13                   ; Set lineEnd to current area X offset + screen width in tiles
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
     adc currentAreaWidth
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
     cmp currentAreaHeight
     bcs loademptytile     
     lda areaCol
     cmp currentAreaWidth 
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

lineEnd
    .byte $14

;; ----------------------
;; ANIMATE LEVEL CHARS
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
    sta $21
    
    lda #$31
    sta $20
    jsr leftshift_20
    jsr leftshift_20
    
    lda #$32
    sta $20
    jsr leftshift_20
    jsr leftshift_20

    lda #$35
    sta $20
    jsr rightshift_20
    jsr rightshift_20
    
    lda #$36
    sta $20
    jsr rightshift_20
    jsr rightshift_20
    
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

leftshift_20            ldy #$00                    ; Rotate Left/Bit-shift with wrap
                        lda ($20),y                 ; Operates on the value in zero-page adress $20 
                        asl    
                        bcc leftshift_20_nocarry        
                        ora #$01
leftshift_20_nocarry    sta ($20),y
                        rts


rightshift_20           ldy #$00                    ; Rotate Right/Bit-shift with wrap
                        lda ($20),y                 ; Operates on the value in zero-page adress $20
                        lsr
                        bcc rightshift_20_nocarry        
                        ora #$80
rightshift_20_nocarry
                        sta ($20),y
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

;; ----------------------
;; SCENE STATE
;; ----------------------
currentAreaOffsetX  .byte $02
currentAreaOffsetY  .byte $00

screenDirty .byte $00

;; ----------------------
;; LEVEL DATA
;; ----------------------

*=$8000

currentAreaWidth = #$17
currentAreaHeight = #$17

currentArea
     .byte $05, $04, $03, $04, $05, $05, $04, $04, $04, $02, $02, $04, $04, $04, $05, $05, $05, $05, $04, $05, $05, $05, $05
     .byte $05, $05, $03, $04, $04, $04, $05, $04, $04, $02, $02, $04, $04, $04, $04, $04, $04, $01, $05, $05, $04, $05, $04
     .byte $04, $04, $03, $05, $04, $04, $05, $05, $04, $02, $02, $04, $04, $04, $04, $04, $05, $05, $04, $05, $05, $05, $05
     .byte $05, $04, $03, $04, $04, $01, $05, $04, $04, $02, $02, $02, $04, $04, $04, $05, $04, $04, $04, $05, $05, $05, $05
     .byte $04, $04, $03, $03, $04, $04, $04, $04, $04, $04, $02, $02, $04, $04, $04, $05, $04, $04, $04, $04, $04, $05, $04
     .byte $05, $04, $04, $03, $05, $04, $04, $01, $04, $04, $02, $02, $04, $05, $05, $04, $04, $05, $04, $05, $04, $04, $05
     .byte $04, $04, $04, $03, $03, $04, $04, $04, $04, $02, $02, $04, $04, $04, $05, $04, $04, $04, $04, $05, $05, $05, $05
     .byte $04, $04, $04, $04, $03, $03, $05, $04, $04, $02, $02, $04, $04, $05, $04, $04, $01, $05, $04, $04, $05, $04, $04
     .byte $05, $04, $04, $04, $04, $03, $03, $03, $04, $02, $02, $04, $04, $04, $04, $04, $04, $04, $04, $04, $05, $05, $05
     .byte $04, $04, $05, $04, $04, $04, $04, $03, $03, $03, $03, $03, $03, $03, $03, $04, $04, $04, $05, $05, $05, $04, $05
     .byte $05, $05, $04, $04, $05, $04, $05, $04, $01, $02, $02, $04, $04, $04, $03, $03, $03, $03, $03, $03, $03, $03, $03
     .byte $05, $04, $05, $04, $05, $04, $04, $04, $05, $02, $02, $05, $05, $04, $05, $05, $04, $05, $04, $04, $05, $05, $04
     .byte $04, $04, $05, $05, $04, $05, $05, $04, $05, $02, $02, $04, $05, $05, $05, $04, $04, $05, $05, $05, $05, $05, $05
     .byte $04, $05, $04, $05, $05, $04, $05, $05, $04, $02, $02, $04, $04, $05, $04, $04, $04, $04, $05, $04, $05, $04, $04 
     .byte $04, $04, $04, $05, $04, $05, $05, $04, $02, $02, $04, $05, $04, $04, $05, $04, $04, $04, $04, $05, $05, $04, $04 
     .byte $04, $05, $05, $04, $05, $04, $04, $04, $02, $02, $04, $04, $04, $04, $04, $04, $04, $05, $04, $05, $04, $05, $05 
     .byte $04, $04, $04, $05, $04, $04, $04, $02, $02, $02, $04, $04, $04, $05, $04, $04, $04, $05, $05, $05, $04, $04, $05 
     .byte $04, $04, $04, $04, $04, $02, $02, $02, $02, $04, $05, $04, $05, $05, $05, $04, $04, $04, $04, $04, $05, $05, $04 
     .byte $04, $04, $04, $04, $02, $02, $02, $02, $04, $04, $04, $04, $05, $05, $05, $05, $04, $04, $05, $05, $04, $04, $04 
     .byte $04, $05, $04, $04, $02, $02, $04, $04, $04, $04, $04, $04, $04, $04, $05, $05, $04, $04, $05, $04, $05, $04, $05 
     .byte $04, $04, $04, $02, $02, $04, $04, $04, $04, $04, $05, $05, $05, $04, $05, $04, $04, $04, $04, $04, $05, $04, $05 
     .byte $04, $05, $04, $02, $02, $04, $04, $04, $04, $04, $04, $04, $04, $05, $04, $04, $04, $04, $05, $04, $04, $04, $04 
     .byte $04, $04, $04, $02, $02, $04, $04, $04, $04, $04, $04, $04, $05, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04 

;; ----------------------
;; TILE DATA
;; ----------------------

*=$3000
icons
     .byte $48, $48, $48, $48 ;; Nothing/Black
     .byte $47, $20, $20, $47 ;; Rocks
     .byte $46, $46, $46, $46 ;; Water
     .byte $44, $45, $45, $44 ;; Road
     .text "    "             ;; Background
     .byte $43, $42, $40, $41 ;; Tree
     .byte $48, $48, $48, $48 ;; Nothing/Black 2 test
     
iconcols 
     .byte $00, $00, $00, $00
     .byte $01, $01, $01, $01
     .byte $1e, $1e, $1e, $1e
     .byte $1d, $1d, $1d, $1d
     .byte $1d, $1d, $1d, $1d
     .byte $1d, $1d, $1d, $1d
     .byte $1d, $1d, $1d, $1d
     .byte $1d, $1d, $1d, $1d


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



*=$2000
.binary "spritedata.raw"

*=$3800
.binary "gamechars-charset.bin" 

; 15 bytes