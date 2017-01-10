;; +----------------------------------+
;; |                                  |
;; |    MATH CONSTANTS & ROUTINES     |
;; |                                  |
;; +----------------------------------+

;; +----------------------------------------+
;; | QUICK LOOKUP-TABLES FOR MULTIPLICATION |
;; +----------------------------------------+
powersOf2     .byte $00 ;0
              .byte $02 ;1
              .byte $04 ;2
              .byte $06 ;3
              .byte $08 ;4
              .byte $0a ;5

powersOf16    .byte $00 ;0
              .byte $10 ;1
              .byte $20 ;2
              .byte $30 ;3
              .byte $40 ;4
              .byte $50 ;5
              .byte $60 ;6
              .byte $70 ;7
              .byte $80 ;8
              .byte $90 ;9
              .byte $a0 ;10
              .byte $b0 ;11
              .byte $c0 ;12
              .byte $d0 ;13
              .byte $e0 ;14
              .byte $f0 ;15
              .byte $00 ;16
              .byte $10 ;17
              .byte $20 ;18
              .byte $30 ;19


powersOf20    .byte $00 ;0
              .byte $14 ;1
              .byte $28 ;2
              .byte $3c ;3
              .byte $50 ;4
              .byte $64 ;5
              .byte $78 ;6
              .byte $8c ;7
              .byte $a0 ;8
              .byte $b4 ;9
              .byte $c8 ;10

tmpPtr1       .byte $00, $00
tmpPtr2       .byte $00, $00


;; +----------------------------------------+
;; | MATH ROUTINE INPUTS                    |
;; +----------------------------------------+

num1          .byte $00                ; Math input #1
num2          .byte $00                ; Math input #2
num3          .byte $00                ; Math input #3

;; +----------------------------------------+
;; | RANDOMIZATION                          |
;; +----------------------------------------+
seed          .byte $01
              .byte $02
              .byte $03
seedPad       .byte $00

rndDivisorTable:
        .byte $ff       ; 1
        .byte $80       ; 2
        .byte $55       ; 3
        .byte $40       ; 4
        .byte $33       ; 5
        .byte $2b       ; 6
        .byte $24       ; 7
        .byte $20       ; 8
        .byte $1c       ; 9
        .byte $1a       ; 10
        .byte $17       ; 11
        .byte $15       ; 12
        .byte $14       ; 13
        .byte $12       ; 14
        .byte $11       ; 15
        .byte $10       ; 16
        .byte $0f       ; 17
        .byte $0e       ; 18
        .byte $0d       ; 19
        .byte $0d       ; 20
        .byte $0c       ; 21
        .byte $0c       ; 22
        .byte $0b       ; 23
        .byte $0b       ; 24
        .byte $0a       ; 25
        .byte $0a       ; 26
        .byte $09       ; 27
        .byte $09       ; 28
        .byte $09       ; 29
        .byte $09       ; 30
        .byte $08       ; 31
        .byte $08       ; 32
        .byte $08       ; 33
        .byte $08       ; 34
        .byte $07       ; 35
        .byte $07       ; 36
        .byte $07       ; 37
        .byte $07       ; 38
        .byte $07       ; 39
        .byte $06       ; 40
        .byte $06       ; 41
        .byte $06       ; 42
        .byte $06       ; 43
        .byte $06       ; 44
        .byte $06       ; 45
        .byte $06       ; 46

rndInt:
        ldx num3
        lda rndDivisorTable, x
        sta num2
        jsr rndNum
        sta num1
        jsr divide_rndup
        rts

rndNum:     lda seed
            and #%00000001
            cmp #%00000001
            bne rndNmNoShf
            lda seed+2
            sta seedPad
            lda seed+1
            sta seed+2
            lda seed
            sta seed+1
            lda seedPad
            sta seed
rndNmNoShf  lda seed
            beq doEor
            asl
            beq noEor ;if the input was $80, skip the EOR
            bcc noEor
doEor:      eor #$1d
noEor:      sta seed
            rts

; num1 - input number
; num2 - divide by factor ( X = 256 / num2 )
; num3 - X
divide_rndup:
                 ldy #$00   ; loop counter
                 lda num2   ; testreg
divrndup_loop    cmp num1
                 bcs divrndup_end
                 clc
                 adc num2
                 iny
                 cpy num3
                 beq divrndup_end
                 jmp divrndup_loop

divrndup_end     tya
                 rts


;; +----------------------------------------+
;; | MULTIPLICATION                          |
;; +----------------------------------------+

multiply         lda #$00            ; Multiply with carry 
                 beq mply_enterLoop

mply_doAdd       clc
                 adc num1

mply_loop        asl num1
mply_enterLoop   lsr num2
                 bcs mply_doAdd
                 bne mply_loop
                 rts

;; +----------------------------------------+
;; | POINTER MANIPULATION                   |
;; +----------------------------------------+

inc20ModVal .byte $00
inc22ModVal .byte $00
rollIterations .byte $00
modVal .byte $00

inc20Ptr:           ; Helper subroutine for increasing 16-bit buffer value at $20-21
                    lda $20
                    clc
                    adc inc20ModVal
                    sta $20
                    bcs inc20Carry
                    rts
inc20Carry          inc $21
                    rts

dec20Ptr:           ; Helper subroutine for decreasing 16-bit buffer value at $20-21
                    lda $20
                    clc
                    sbc inc20ModVal
                    sta $20
                    bcc dec20Carry
                    rts
dec20Carry          dec $21
                    rts


inc22Ptr:           ; Helper subroutine for increasing 16-bit buffer value at $22-23
                    lda $22
                    clc
                    adc inc22ModVal
                    sta $22
                    bcs inc22Carry
                    rts
inc22Carry          inc $23
                    rts

dec22Ptr:           ; Helper subroutine for decreasing 16-bit buffer value at $20-21
                    lda $22
                    clc
                    sbc inc22ModVal
                    sta $22
                    bcc dec22Carry
                    rts
dec22Carry          dec $23
                    rts

roll22Ptr           ldx #$00
roll22PtrLoop       cpx rollIterations
                    beq rollEnd
                    jsr inc22Ptr
                    inx
                    jmp roll22PtrLoop
rollEnd             rts
