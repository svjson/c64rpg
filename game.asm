*=$0801
.byte $0c, $08, $0a, $00, $9e, $20
.byte $34, $30, $39, $36, $00, $00
.byte $00

;; ------------
;; INIT ROUTINE
;; ------------
*=$1000

     lda #$00           ; Set screen color
     sta $D020
     lda #$05
     sta $D021
     
     lda #$09           ; Set character set color
     sta $D022
     lda #$1d
     sta $D023
          
     lda $d018          ; Remap character set
     ora #$0e
     sta $d018
     
     jsr clearscreen
     
     lda #$18           ; Char multicolour mode on 
     sta $d016    

     lda #$00
     sta $fb            ; raster counter
     
     lda #$01           ; Enable player sprite
     sta $07f8
     lda #$01
     sta $d015
     sta $d027
     lda #$33
     sta $d000
     sta $d001
     
     sei                 ;enable interrupts

     
     jmp mainloop

*=6000    
charactersettest
     tax    
     sta $0400, x
     adc #$01
     cmp #$ff
     bne charactersettest
     rts

;; ------------
;; MAIN LOOP
;; ------------
         
mainloop            lda #$00     ; wait for raster retrace
                    cmp $d012  
                    bne mainloop

                    inc $fb      ; wait for anim/delay counter to loop
                    lda #$40
                    cmp $fb
                    bne mainloop 

                    lda #$00     ; reset anim/delay counter
                    sta $fb

                    jsr drawlevel
                    jsr animatechars

                    jmp mainloop
    

;; ----------------------
;; LEVEL DRAWING ROUTINES
;; ----------------------

drawlevel          
     lda #$04 ; Screen offset
     sta $21
     lda #$00
     sta $20  

     lda #$d8 ; Color offset
     sta $25
     lda #$00
     sta $24
     
     lda #$80 ; Level area offset    ; Data at $2000
     sta $23
     lda #$00
     sta $22
     
     lda #$00     
     sta crsr
     sta iter
     sta drawat
     
drawlevelloop
     inc iter
     ldx iter
     
     jsr drawline          

     jsr incleveloffset
     jsr incscreenoffset

     ldx iter
     cpx #$0C
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
     adc #$14
     bcc levelnocarry
     inc $23
levelnocarry
     sta $22
     rts
     
drawline
     lda #$00
     sta crsr
     sta drawat
drawlineloop
; Load source block index
     ldy crsr
     lda ($22), y
     
; Multiply by 4 to jump to source block data and put start pos in Y
     sta num1
     lda #$04
     sta num2
     jsr multiply
     tax
     
; Draw to screen 
     ldy drawat     

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
     
     cpy #$14
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
;; LEVEL DATA
;; ----------------------

*=$8000
testarea
     .byte $05, $04, $03, $04, $05, $05, $04, $04, $04, $02, $02, $04, $04, $04, $05, $05, $05, $05, $04, $05
     .byte $05, $05, $03, $04, $04, $04, $05, $04, $04, $02, $02, $04, $04, $04, $04, $04, $04, $01, $05, $05
     .byte $04, $04, $03, $05, $04, $04, $05, $05, $04, $02, $02, $04, $04, $04, $04, $04, $05, $05, $04, $05
     .byte $05, $04, $03, $04, $04, $01, $05, $04, $04, $02, $02, $02, $04, $04, $04, $05, $04, $04, $04, $05
     .byte $04, $04, $03, $03, $04, $04, $04, $04, $04, $04, $02, $02, $04, $04, $04, $05, $04, $04, $04, $04
     .byte $05, $04, $04, $03, $05, $04, $04, $01, $04, $04, $02, $02, $04, $05, $05, $04, $04, $05, $04, $05
     .byte $04, $04, $04, $03, $03, $04, $04, $04, $04, $02, $02, $04, $04, $04, $05, $04, $04, $04, $04, $05
     .byte $04, $04, $04, $04, $03, $03, $05, $04, $04, $02, $02, $04, $04, $05, $04, $04, $01, $05, $04, $04
     .byte $05, $04, $04, $04, $04, $03, $03, $03, $04, $02, $02, $04, $04, $04, $04, $04, $04, $04, $04, $04
     .byte $04, $04, $05, $04, $04, $04, $04, $03, $03, $03, $03, $03, $03, $03, $03, $04, $04, $04, $05, $05
     .byte $05, $05, $04, $04, $05, $04, $05, $04, $01, $02, $02, $04, $04, $04, $03, $03, $03, $03, $03, $03
     .byte $05, $04, $05, $04, $05, $04, $04, $04, $05, $02, $02, $05, $05, $04, $05, $05, $04, $05, $04, $04


;; ----------------------
;; TILE DATA
;; ----------------------

*=$3000
icons
     .byte $40, $00, $00, $00 ;; Unused
     .byte $47, $20, $20, $47 ;; Rocks
     .byte $46, $46, $46, $46 ;; Water
     .byte $44, $45, $45, $44 ;; Road
     .text "    "             ;; Background
     .byte $43, $42, $40, $41 ;; Tree
     .byte $00, $00, $00, $00 ;; Unused
     
iconcols 
     .byte $1d, $1d, $1d, $1d
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

multiply         lda #$00            ; Multiply with carry
                 beq mply_enterLoop

mply_doAdd       clc
                 adc num1

mply_loop        asl num1
mply_enterLoop   lsr num2
                 bcs mply_doAdd
                 bne mply_loop
                 rts



*=$3800
.binary "gamechars-charset.bin" 

*=$2000
.binary "spritedata.raw"

; 15 bytes