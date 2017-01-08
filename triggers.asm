;; +----------------------------------+
;; |                                  |
;; |    TRIGGER HANDLING              |
;; |                                  |
;; +----------------------------------+
var_triggerType_AUTO_EXIT__COORD     = #$01
var_triggerType_AUTO_EXIT__INDEX     = #$02
var_triggerType_ACTIVATE_EXIT__COORD = #$03
var_triggerType_ACTIVATE_EXIT__INDEX = #$04

var_triggerAction_WALK_ON            = #$01
var_triggerAction_MAN_ACTIVATE       = #$02

resp_trigger_NO_TRIGGER              = #$00
resp_trigger_EXIT_TRIGGERED          = #$01

trAction .byte $00
trTargetIndex .byte $ff

;; +----------------------------------+
;; |    TRIGGER EVALUATION            |
;; +----------------------------------+
;
; Input - playerX - Trigger location X
;         playerY - Trigger location Y
;         trAction - Trigger Action
; Response - resp_trigger value in A
;
cTriggerType .byte $00

evaluateTriggerAt:
                    ldy #$00
triggerIterLoop     cpy triggerTableSize
                    beq noTriggerResponse

                    sty num1
                    lda #$07
                    sta num2
                    jsr multiply

                    tax
                    lda triggerTable, x
                    cmp playerX
                    bne nextTrigger

                    inx
                    lda triggerTable, x
                    cmp playerY
                    bne nextTrigger

executeTrigger      inx
                    lda triggerTable, x     ; Trigger type. Ignore for now
                    sta cTriggerType

                    cmp #$01
                    bne etEv2
                    jmp executeExitAutoCoordTrigger
etEv2               cmp #$02
                    bne etEv3
                    jmp executeExitActivateCoordTrigger
etEv3               cmp #$03
                    bne etEv4
                    jmp executeExitAutoIndexTrigger
etEv4               jmp executeExitActivateIndexTrigger

nextTrigger         iny
                    jmp triggerIterLoop

noTriggerResponse   lda resp_trigger_NO_TRIGGER
                    rts

executeExitAutoCoordTrigger:
                    lda trAction
                    cmp var_triggerAction_WALK_ON
                    bne noTriggerResponse
                    jsr evalTriggerTarget
                    jmp finalizeExitCoordTrigger

executeExitActivateCoordTrigger:
                    lda trAction
                    cmp var_triggerAction_MAN_ACTIVATE
                    bne noTriggerResponse
                    jsr evalTriggerTarget
                    jmp finalizeExitCoordTrigger

executeExitAutoIndexTrigger:
                    lda trAction
                    cmp var_triggerAction_WALK_ON
                    bne noTriggerResponse
                    jsr evalTriggerTarget
                    jmp finalizeExitIndexTrigger

executeExitActivateIndexTrigger:
                    lda trAction
                    cmp var_triggerAction_MAN_ACTIVATE
                    bne noTriggerResponse
                    jsr evalTriggerTarget
                    jmp finalizeExitIndexTrigger

evalTriggerTarget:
                    inx
                    lda triggerTable, x     ; Area addr lo-byte
                    sta $20
                    inx
                    lda triggerTable, x     ; Area addr hi-byte
                    sta $21

                    lda playerX
                    sta prevPlayerX
                    lda playerY
                    sta prevPlayerY
                    rts

finalizeExitCoordTrigger:
                    inx
                    lda triggerTable, x     ; Target X coord
                    sta playerX
                    inx
                    lda triggerTable, x     ; Target Y coord
                    sta playerY
                    lda #$ff
                    sta trTargetIndex
                    lda resp_trigger_EXIT_TRIGGERED
                    rts

finalizeExitIndexTrigger:
                    inx
                    lda triggerTable, x     ; Target Index
                    sta trTargetIndex
                    lda resp_trigger_EXIT_TRIGGERED

;; +----------------------------------+
;; |    TRIGGER LOOKUP                |
;; +----------------------------------+

forwardTriggerTableToIndexA:
                    sta rollIterations
                    lda #<triggerTable
                    sta $22
                    lda #>triggerTable
                    sta $23
                    lda triggerTableRowSize
                    sta inc22ModVal
                    jsr roll22Ptr
                    rts

