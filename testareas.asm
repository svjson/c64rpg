;; +----------------------------------+
;; |                                  |
;; |    TEST AREAS                    |
;; |                                  |
;; +----------------------------------+

;; +----------------------------------+
;; |    FIRST OUTSIDE AREA            |
;; +----------------------------------+
outsidearea
     .byte $28  ; width
     .byte $17  ; height
     .byte $00  ; area mode
     .byte %00001110; tileset mask
     .byte $05, $09, $1d
     .byte $05, $04, $03, $04, $05, $05, $04, $04, $04, $02, $02, $04, $0e, $0c, $05, $05, $05, $05, $04, $05, $05, $05, $05, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
     .byte $05, $05, $0a, $04, $04, $04, $05, $04, $0c, $02, $02, $0d, $04, $04, $04, $04, $04, $01, $05, $05, $04, $05, $04, $04, $04, $04, $04, $05, $04, $04, $04, $05, $0c, $04, $04, $04, $04, $04, $04, $04
     .byte $04, $04, $03, $05, $04, $04, $11, $05, $04, $02, $02, $04, $0b, $04, $04, $04, $11, $05, $04, $05, $11, $05, $05, $04, $05, $04, $04, $04, $04, $04, $05, $05, $04, $04, $04, $04, $04, $04, $04, $04
     .byte $05, $04, $0a, $04, $0b, $01, $05, $04, $0e, $02, $02, $02, $04, $0f, $04, $06, $04, $04, $04, $05, $05, $11, $05, $04, $04, $04, $04, $06, $05, $05, $06, $04, $04, $05, $04, $04, $05, $04, $04, $04
     .byte $04, $04, $0a, $03, $0c, $04, $04, $04, $04, $04, $02, $02, $04, $04, $04, $06, $04, $04, $0d, $0c, $0b, $05, $04, $06, $11, $04, $04, $04, $0d, $0e, $05, $04, $04, $05, $04, $04, $05, $04, $04, $04
     .byte $05, $04, $04, $0a, $05, $04, $04, $01, $04, $04, $02, $02, $04, $06, $05, $04, $04, $05, $04, $05, $04, $04, $05, $04, $04, $05, $04, $06, $06, $05, $04, $06, $04, $04, $05, $04, $04, $04, $04, $04
     .byte $04, $04, $04, $03, $0a, $0d, $0e, $04, $0d, $02, $02, $0c, $04, $04, $11, $0e, $04, $04, $04, $05, $05, $05, $05, $04, $0d, $05, $05, $04, $04, $04, $04, $04, $0c, $04, $05, $04, $05, $04, $04, $04
     .byte $04, $04, $04, $04, $03, $03, $05, $04, $04, $02, $02, $0b, $04, $05, $0b, $0c, $0d, $05, $04, $04, $05, $04, $04, $04, $0c, $04, $05, $04, $04, $04, $0e, $04, $04, $04, $05, $04, $05, $04, $04, $04
     .byte $05, $04, $04, $04, $04, $0a, $03, $0a, $04, $02, $02, $04, $04, $04, $04, $0b, $04, $04, $0b, $04, $05, $05, $05, $04, $0b, $04, $04, $04, $04, $04, $0d, $05, $0b, $04, $04, $04, $04, $04, $04, $04
     .byte $04, $04, $05, $04, $04, $04, $04, $0a, $03, $12, $12, $03, $0a, $03, $03, $04, $04, $04, $05, $05, $05, $04, $05, $04, $04, $04, $0b, $04, $0f, $04, $0c, $05, $04, $04, $04, $04, $04, $04, $04, $04
     .byte $05, $05, $04, $04, $05, $04, $11, $04, $01, $02, $02, $04, $04, $04, $0a, $0a, $03, $0a, $03, $0a, $03, $0a, $03, $03, $04, $04, $0c, $0e, $04, $04, $04, $04, $05, $04, $05, $04, $04, $04, $04, $04
     .byte $05, $04, $05, $04, $06, $04, $04, $0f, $05, $02, $02, $05, $05, $04, $05, $05, $04, $05, $04, $04, $11, $05, $04, $0a, $03, $04, $0d, $04, $04, $04, $04, $04, $05, $04, $05, $04, $05, $04, $04, $04
     .byte $04, $0b, $05, $05, $04, $11, $05, $04, $05, $02, $02, $04, $11, $05, $05, $04, $04, $05, $05, $05, $05, $05, $11, $04, $0a, $04, $08, $08, $08, $08, $08, $08, $05, $04, $04, $04, $05, $04, $04, $04
     .byte $04, $05, $04, $05, $11, $04, $11, $05, $04, $02, $02, $04, $0b, $05, $04, $04, $04, $04, $05, $04, $11, $04, $0b, $04, $03, $04, $08, $08, $08, $08, $08, $08, $04, $04, $0c, $04, $04, $04, $04, $04
     .byte $0d, $04, $04, $05, $04, $05, $11, $04, $02, $02, $04, $05, $0c, $0b, $11, $04, $04, $0f, $04, $05, $05, $04, $0d, $04, $0a, $04, $08, $08, $08, $08, $08, $08, $04, $0e, $0f, $04, $04, $0a, $0a, $0a
     .byte $04, $05, $05, $04, $11, $04, $04, $04, $02, $02, $0f, $0c, $0e, $04, $04, $04, $04, $05, $04, $11, $04, $11, $05, $0b, $0a, $04, $08, $08, $08, $08, $08, $08, $04, $0d, $04, $04, $0a, $03, $04, $04
     .byte $04, $04, $04, $05, $04, $04, $04, $02, $02, $02, $04, $0d, $04, $11, $04, $04, $04, $05, $05, $05, $04, $04, $11, $0c, $03, $04, $07, $07, $07, $09, $07, $07, $04, $04, $04, $04, $0a, $05, $04, $04
     .byte $04, $04, $04, $04, $04, $02, $02, $02, $02, $04, $05, $04, $06, $05, $05, $04, $04, $04, $04, $04, $05, $05, $04, $04, $0a, $0a, $04, $06, $04, $0a, $04, $05, $04, $04, $0a, $03, $03, $04, $04, $04
     .byte $0b, $04, $04, $04, $02, $02, $02, $02, $04, $04, $04, $04, $06, $06, $11, $05, $04, $04, $05, $05, $04, $04, $04, $04, $04, $03, $03, $0a, $0a, $03, $03, $0a, $03, $03, $0a, $04, $04, $05, $05, $04
     .byte $04, $05, $04, $04, $02, $02, $04, $04, $04, $0b, $04, $04, $04, $04, $11, $05, $01, $04, $05, $04, $05, $04, $11, $04, $04, $04, $04, $04, $0f, $04, $04, $04, $04, $04, $04, $05, $05, $04, $05, $04
     .byte $0d, $04, $04, $02, $02, $04, $04, $0d, $0e, $0c, $05, $05, $05, $04, $05, $0b, $0f, $10, $04, $04, $05, $04, $05, $04, $04, $0b, $0e, $0d, $05, $06, $04, $0f, $04, $05, $04, $05, $05, $04, $05, $04
     .byte $04, $05, $01, $02, $02, $04, $04, $04, $04, $0d, $04, $04, $04, $11, $04, $0e, $0c, $04, $05, $01, $04, $04, $04, $04, $0b, $0c, $0c, $0e, $04, $05, $04, $04, $04, $04, $05, $04, $05, $04, $04, $04
     .byte $04, $04, $04, $02, $02, $04, $04, $04, $04, $04, $04, $04, $05, $04, $04, $04, $0d, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04, $04
     ;---
     .byte $01 ; - Number of triggers
     ;---
     .byte $1d  ; Trigger X
     .byte $10  ; Trigger Y
     .byte $01
     .byte <(houseArea)
     .byte >(houseArea)
     .byte $04  ; Target X
     .byte $06  ; Target Y
     ;---
     .byte $00 ; NPC table size
     ;---
     .byte $00 ; Item table size
     .fill $46

;; +----------------------------------+
;; |    FIRST CAVE AREA               |
;; +----------------------------------+
dungeoncellar
     .byte $21  ; width
     .byte $17  ; height
     .byte %10000000
     .byte %00001010    ; Tile set mask
     .byte $1f, $1c, $1b
     .byte $0d, $0d, $07, $04, $04, $04, $04, $04, $04, $04, $04, $04, $0e, $0d, $0d, $0d, $0d, $07, $04, $04, $04, $04, $04, $04, $0e, $0d, $0d, $07, $04, $04, $0e, $0d, $0d
     .byte $0d, $0d, $05, $01, $02, $03, $01, $01, $02, $03, $02, $01, $08, $04, $04, $04, $04, $09, $01, $03, $03, $01, $01, $02, $08, $0e, $0d, $05, $01, $02, $08, $0e, $0d
     .byte $0d, $0d, $05, $03, $02, $0f, $01, $0b, $0a, $0c, $02, $01, $01, $01, $01, $01, $01, $01, $02, $01, $02, $02, $03, $01, $02, $06, $0d, $05, $03, $01, $01, $06, $0d
     .byte $0d, $07, $09, $01, $03, $10, $01, $06, $0d, $05, $03, $03, $0b, $0a, $0a, $0a, $0c, $03, $0b, $0a, $0a, $0a, $0c, $03, $01, $08, $0e, $07, $15, $02, $03, $08, $0e
     .byte $0d, $05, $03, $02, $01, $10, $01, $06, $0d, $05, $01, $01, $06, $07, $04, $04, $09, $02, $08, $04, $04, $0e, $05, $01, $02, $01, $06, $05, $01, $03, $01, $01, $06
     .byte $0d, $05, $01, $0b, $0a, $05, $01, $06, $0d, $05, $02, $01, $06, $05, $01, $03, $02, $01, $01, $1a, $1a, $06, $05, $02, $03, $01, $14, $09, $03, $01, $02, $03, $06
     .byte $0d, $05, $02, $08, $04, $09, $01, $08, $0e, $05, $02, $03, $06, $05, $03, $02, $01, $01, $01, $02, $1a, $06, $05, $01, $03, $02, $13, $03, $02, $03, $02, $01, $06
     .byte $07, $09, $03, $01, $01, $01, $01, $02, $06, $05, $03, $01, $06, $05, $01, $02, $03, $02, $01, $03, $03, $06, $05, $01, $0f, $01, $03, $01, $01, $0b, $0a, $0a, $0d
     .byte $05, $01, $01, $03, $01, $0b, $0a, $0a, $0d, $05, $01, $01, $06, $05, $03, $01, $02, $01, $03, $01, $02, $06, $05, $01, $06, $0c, $01, $01, $02, $08, $04, $04, $0e
     .byte $0d, $0c, $03, $01, $01, $08, $04, $04, $0e, $05, $02, $02, $06, $05, $03, $01, $01, $02, $01, $03, $01, $06, $07, $12, $04, $09, $01, $03, $03, $01, $01, $01, $06
     .byte $0d, $05, $02, $02, $01, $01, $03, $01, $06, $05, $01, $01, $06, $05, $19, $02, $03, $02, $01, $02, $02, $06, $05, $01, $01, $01, $01, $03, $0b, $0c, $03, $0b, $0d
     .byte $0d, $0d, $0c, $01, $03, $01, $03, $01, $06, $05, $03, $02, $06, $05, $02, $01, $01, $01, $02, $01, $03, $06, $05, $03, $02, $03, $02, $18, $04, $09, $02, $08, $0e
     .byte $0d, $0d, $0d, $0c, $01, $02, $01, $01, $06, $05, $02, $03, $06, $05, $01, $01, $01, $03, $01, $01, $01, $06, $05, $02, $02, $03, $02, $10, $02, $01, $01, $01, $06
     .byte $0d, $0d, $07, $09, $03, $01, $02, $03, $08, $09, $01, $02, $08, $09, $03, $0b, $0a, $0a, $0a, $0a, $0a, $0d, $05, $01, $01, $01, $01, $10, $01, $0f, $03, $02, $06
     .byte $0d, $0d, $05, $01, $02, $03, $01, $01, $03, $03, $01, $01, $01, $01, $01, $06, $0d, $07, $04, $0e, $0d, $0d, $05, $02, $03, $02, $0b, $05, $03, $10, $02, $0b, $0d
     .byte $0d, $0d, $05, $03, $01, $02, $02, $03, $11, $12, $16, $12, $17, $0c, $03, $06, $0d, $05, $02, $06, $0d, $0d, $05, $01, $02, $01, $06, $05, $03, $10, $01, $08, $0e
     .byte $0d, $07, $09, $02, $03, $01, $01, $01, $01, $02, $10, $03, $08, $09, $01, $08, $04, $09, $03, $08, $04, $04, $09, $02, $02, $03, $14, $09, $02, $10, $02, $03, $06
     .byte $0d, $05, $02, $01, $01, $03, $02, $02, $01, $02, $06, $0c, $01, $03, $01, $01, $02, $01, $01, $03, $01, $02, $02, $03, $0b, $1e, $09, $01, $02, $14, $15, $02, $06
     .byte $0d, $05, $01, $02, $01, $01, $01, $01, $0f, $01, $06, $05, $02, $02, $02, $01, $0b, $0a, $0c, $01, $01, $01, $18, $12, $04, $09, $01, $01, $18, $09, $01, $01, $06
     .byte $0d, $0d, $0c, $01, $03, $03, $01, $01, $10, $03, $06, $05, $03, $01, $03, $01, $06, $0d, $05, $01, $03, $0b, $05, $01, $01, $02, $01, $02, $10, $01, $03, $01, $06
     .byte $0d, $0d, $05, $01, $02, $03, $01, $01, $10, $01, $08, $09, $01, $0b, $0a, $0a, $0d, $0d, $0d, $0a, $0a, $0d, $0d, $0c, $02, $03, $03, $02, $10, $1b, $03, $02, $06
     .byte $0d, $0d, $0d, $0a, $0a, $0a, $0c, $01, $10, $01, $03, $01, $02, $06, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0c, $01, $01, $01, $10, $01, $01, $0b, $0d
     .byte $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0a, $0d, $0a, $0a, $0a, $0a, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0d, $0a, $0a, $0a, $0d, $0a, $0a, $0d, $0d
     ;---
     .byte $02 ; - Number of triggers
     ;---
     .byte $0e  ; Trigger X
     .byte $0a  ; Trigger Y
     .byte $02  ; trigger type
     .byte <(houseArea)
     .byte >(houseArea)
     .byte $06  ; Target X
     .byte $01  ; Target Y

     .byte $1d  ; Trigger X
     .byte $14  ; Trigger Y
     .byte $04  ; trigger type
     .byte <(rnddungeon1)
     .byte >(rnddungeon1)
     .byte $00  ; Target Trigger Index
     .byte $00  ; N/A

     ;---
     .byte $0a ; Number of npcs

     .byte %10000000
     .byte $0f, $08         ;; X and Y pos
     .byte $30              ;; Tile ID
     .byte $89              ;; Sprite pointer   $00 = off
     .word npcname_GIANT_RAT ;; Name pointer
     .byte $0c              ;; HP
     .byte $01              ;; Mode
     .byte 0, 0             ;; Target X and Y pos
     .byte 15
     .byte 0

     .byte %10000000
     .byte $04, $04         ;; X and Y pos
     .byte $30              ;; Tile ID
     .byte $89              ;; Sprite pointer   $00 = off
     .word npcname_GIANT_RAT ;; Name pointer
     .byte $0c              ;; HP
     .byte $01              ;; Mode
     .byte 0, 0             ;; Target X and Y pos
     .byte 15
     .byte 0

     .byte %10000000
     .byte $12, $09         ;; X and Y pos
     .byte $30              ;; Tile ID
     .byte $89              ;; Sprite pointer   $00 = off
     .word npcname_GIANT_RAT ;; Name pointer
     .byte $0c              ;; HP
     .byte $01              ;; Mode
     .byte 0, 0             ;; Target X and Y pos
     .byte 15
     .byte 0

     .byte %10000000
     .byte $0e, $13         ;; X and Y pos
     .byte $30              ;; Tile ID
     .byte $89              ;; Sprite pointer   $00 = off
     .word npcname_GIANT_RAT ;; Name pointer
     .byte $0c              ;; HP
     .byte $01              ;; Mode
     .byte 0, 0             ;; Target X and Y pos
     .byte 15
     .byte 0

     .byte %10000000
     .byte 27, 8            ;; X and Y pos
     .byte $31              ;; Tile ID
     .byte $8b              ;; Sprite pointer   $00 = off
     .word npcname_SKELETON_WARRIOR ;; Name pointer
     .byte $12              ;; HP
     .byte $01              ;; Mode
     .byte 0, 0             ;; Target X and Y pos
     .byte 20
     .byte 0

     .byte %10000000
     .byte 30, 19           ;; X and Y pos
     .byte $32              ;; Tile ID
     .byte $8d              ;; Sprite pointer   $00 = off
     .word npcname_KOBOLD   ;; Name pointer
     .byte $06              ;; HP
     .byte $01              ;; Mode
     .byte 0, 0             ;; Target X and Y pos
     .byte 11
     .byte 0

     .byte %10000000
     .byte 30, 20           ;; X and Y pos
     .byte $32              ;; Tile ID
     .byte $8d              ;; Sprite pointer   $00 = off
     .word npcname_KOBOLD   ;; Name pointer
     .byte $06              ;; HP
     .byte $01              ;; Mode
     .byte 0, 0             ;; Target X and Y pos
     .byte 11
     .byte 0

     .byte %10000000
     .byte 25, 19           ;; X and Y pos
     .byte $32              ;; Tile ID
     .byte $8d              ;; Sprite pointer   $00 = off
     .word npcname_KOBOLD   ;; Name pointer
     .byte $06              ;; HP
     .byte $01              ;; Mode
     .byte 0, 0             ;; Target X and Y pos
     .byte 11
     .byte 0

     .byte %10000000
     .byte $05, $08         ;; X and Y pos
     .byte $31              ;; Tile ID
     .byte $8b              ;; Sprite pointer   $00 = off
     .word npcname_SKELETON_WARRIOR ;; Name pointer
     .byte $12              ;; HP
     .byte $01              ;; Mode
     .byte 0, 0             ;; Target X and Y pos
     .byte 20
     .byte 0

     .byte %10000000
     .byte $15, $03         ;; X and Y pos
     .byte $33              ;; Tile ID
     .byte $8f              ;; Sprite pointer   $00 = off
     .word npcname_ORC ;; Name pointer
     .byte $20              ;; HP
     .byte $01              ;; Mode
     .byte 0, 0             ;; Target X and Y pos
     .byte 13
     .byte 0
     ;---
     .byte $06 ; Item table size

     .byte %11000001
     .byte $0e, $05         ;; X and Y pos
     .byte $40              ;; Tile ID
     .byte $0f              ;; Item Type - Unknown Scroll
     .byte $00              ;; Unused
     .byte $03              ;; Actual Type - Scroll of Teleport

     .byte %10000000
     .byte $17, $13         ;; X and Y pos
     .byte $42              ;; Tile ID
     .byte $1a              ;; Item Type - Pieces of Gold
     .byte $00              ;; Unused
     .byte $15              ;; Amount

     .byte %10000000
     .byte $18, $13         ;; X and Y pos
     .byte $42              ;; Tile ID
     .byte $1a              ;; Item Type - Pieces of Gold
     .byte $00              ;; Unused
     .byte $09              ;; Amount

     .byte %10000000
     .byte $18, $14         ;; X and Y pos;
     .byte $42              ;; Tile ID
     .byte $1a              ;; Item Type - Pieces of Gold
     .byte $00              ;; Unused
     .byte $19              ;; Amount

     .byte %11000010
     .byte $07, $15         ;; X and Y pos;
     .byte $41              ;; Tile ID
     .byte $19              ;; Item Type - Unknown Potion
     .byte $00              ;; Unused
     .byte $00              ;; Actual Type - Potion of Heal Minor

     .byte %10001100
     .byte $04, $04         ;; X and Y pos;
     .byte $44              ;; Tile ID
     .byte $30              ;; Item Type - Leather Armor
     .byte $00              ;; Unused
     .byte $55              ;; Durability

     .fill $46  ; Padding

;; +----------------------------------+
;; |    FIRST HOUSE INTERIOR          |
;; +----------------------------------+
houseArea
     .byte $08 ; Area width
     .byte $08 ; Area height
     .byte %10000000
     .byte %00001100    ; Tile set mask
     .byte $1f, $08, $09
     .byte $00, $06, $02, $02, $02, $02, $02, $07
     .byte $00, $03, $01, $01, $01, $01, $10, $04
     .byte $00, $03, $01, $01, $01, $01, $01, $04
     .byte $00, $03, $01, $09, $02, $02, $02, $0c
     .byte $00, $03, $01, $01, $01, $01, $01, $04
     .byte $00, $03, $01, $0e, $0d, $0f, $01, $04
     .byte $00, $03, $01, $01, $01, $01, $01, $04
     .byte $00, $05, $02, $02, $0b, $02, $02, $08
     ;---
     .byte $02  ; Trigger table size
     ;---
     .byte $04  ; Trigger 1 X
     .byte $07  ; Trigger 1 Y
     .byte $01  ; trigger type
     .byte <(outsidearea)
     .byte >(outsidearea)
     .byte $1d  ; Target X
     .byte $11  ; Target Y
     ;---
     .byte $06  ; Trigger 2 X
     .byte $01  ; Trigger 2 Y
     .byte $04  ; trigger type
     .byte <(dungeoncellar)
     .byte >(dungeoncellar)
     .byte $00  ; Target area trigger index
     .byte $00  ; N/A
     ;---
     .byte $00  ; NPC table size
     ;---
     .byte $00  ; Item table size
     .fill $46

;; +----------------------------------+
;; |    RND CAVE LVL1                 |
;; +----------------------------------+
rnddungeon1
     .byte $21 ; W
     .byte $17 ; H
     .byte %11000000
     .byte %00001010    ; Tile set mask
     .byte $1f, $1c, $1b
     .byte $01             ; npcSetIndex
     .byte $01             ; itemSetIndex
     .byte $48             ; Generation Feats #
     .byte $06             ; # of NPCs
     .byte $05             ; # of Items
     ;---
     .byte $02             ; Exit Triggers To Generate
     ;---
     .byte $02             ; Placement Algorithm
     .byte $01             ; N/A, Algorithm Arg1        (or XPos)
     .byte $00             ; N/A, Algorithm Arg2        (or YPos)
     .byte $04             ; Trigger Type
     .byte $19             ; Trigger Tile #
     .byte <(dungeoncellar)
     .byte >(dungeoncellar)
     .byte $01             ; Target area trigger index  (or XPos)
     .byte $00             ; Placement algorithm #      (or YPos)
     ;---
     .byte $02             ; Placement Algorithm
     .byte $01             ; N/A, Algorithm Arg1        (or XPos)
     .byte $00             ; N/A, Algorithm Arg2        (or YPos)
     .byte $04             ; Trigger Type
     .byte $1b             ; Trigger Tile #
     .byte <(rnddungeon2)
     .byte >(rnddungeon2)
     .byte $00             ; Target area trigger index  (or XPos)
     .byte $00             ; Placement algorithm #      (or YPos)

     .fill $450

;; +----------------------------------+
;; |    RND CAVE LVL2                 |
;; +----------------------------------+
rnddungeon2
     .byte $21 ; W
     .byte $17 ; H
     .byte %11000000
     .byte %00001010    ; Tile set mask
     .byte $1f, $1c, $1b
     .byte $01             ; npcSetIndex
     .byte $01             ; itemSetIndex
     .byte $48             ; Generation Feats #
     .byte $06             ; # of NPCs
     .byte $05             ; # of Items
     ;---
     .byte $02             ; Exit Triggers To Generate
     ;---
     .byte $02             ; Placement Algorithm
     .byte $01             ; N/A, Algorithm Arg1        (or XPos)
     .byte $00             ; N/A, Algorithm Arg2        (or YPos)
     .byte $04             ; Trigger Type
     .byte $19             ; Trigger Tile #
     .byte <(rnddungeon1)
     .byte >(rnddungeon1)
     .byte $01             ; Target area trigger index  (or XPos)
     .byte $00             ; N/A                        (or YPos)
     ;---
     .byte $02             ; Placement Algorithm
     .byte $01             ; N/A, Algorithm Arg1        (or XPos)
     .byte $00             ; N/A, Algorithm Arg2        (or YPos)
     .byte $04             ; Trigger Type
     .byte $1b             ; Trigger Tile #
     .byte <(rnddungeon3)
     .byte >(rnddungeon3)
     .byte $00             ; Target area trigger index  (or XPos)
     .byte $00             ; Placement algorithm #      (or YPos)
     .fill $450


;; +----------------------------------+
;; |    RND CAVE LVL3                 |
;; +----------------------------------+
rnddungeon3
     .byte $25 ; W
     .byte $13 ; H
     .byte %11000000
     .byte %00001010    ; Tile set mask
     .byte $1f, $1c, $1b
     .byte $01             ; npcSetIndex
     .byte $01             ; itemSetIndex
     .byte $48             ; Generation Feats #
     .byte $06             ; # of NPCs
     .byte $05             ; # of Items
     ;---
     .byte $01             ; Exit Triggers To Generate
     ;---
     .byte $02             ; Placement Algorithm
     .byte $01             ; N/A, Algorithm Arg1        (or XPos)
     .byte $00             ; N/A, Algorithm Arg2        (or YPos)
     .byte $04             ; Trigger Type
     .byte $19             ; Trigger Tile #
     .byte <(rnddungeon2)
     .byte >(rnddungeon2)
     .byte $01             ; Target area trigger index  (or XPos)
     .byte $00             ; N/A                        (or YPos)

     .fill $450
