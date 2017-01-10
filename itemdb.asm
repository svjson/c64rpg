;; +----------------------------------+
;; |    ITEM NAMES                    |
;; +----------------------------------+
itemname_SCROLL             .byte 06
                            .text "SCROLL"
itemname_POTION             .byte 06
                            .text "POTION"
itemname_SHORT_SWORD        .byte 11
                            .text "SHORT SWORD"
itemname_LONGSWORD          .byte 9
                            .text "LONGSWORD"
itemname_PIECES_OF_GOLD     .byte 14
                            .text "PIECES OF GOLD"
itemname_LEATHER_ARMOR      .byte 13
                            .text "LEATHER ARMOR"
itemname_LEATHER_HELMET     .byte 14
                            .text "LEATHER HELMET"
itemname_IRON_HELMET        .byte 11
                            .text "IRON HELMET"
itemname_STEEL_HELMET       .byte 12
                            .text "STEEL HELMET"
itemname_SMALL_SHIELD       .byte 12
                            .text "SMALL SHIELD"
itemname_LARGE_SHIELD       .byte 12
                            .text "LARGE SHIELD"

;; +----------------------------------+
;; |    ITEM SUBTYPE NAMES            |
;; +----------------------------------+
itemtype_UNKNOWN            .byte 7
                            .text "UNKNOWN"
itemtype_MINOR_HEALING      .byte 13
                            .text "MINOR HEALING"
itemtype_MEDIUM_HEALING     .byte 14
                            .text "MEDIUM HEALING"
itemtype_MAJOR_HEALING      .byte 13
                            .text "MAJOR HEALING"
itemtype_TELEPORT           .byte 8
                            .text "TELEPORT"
itemtype_CURE_POISON        .byte 11
                            .text "CURE POISON"
itemtype_IDENTIFY           .byte 8
                            .text "IDENTIFY"
itemtype_CLARITY            .byte 7
                            .text "CLARITY"
itemtype_WATER              .byte 5
                            .text "WATER"
itemtype_POISON             .byte 6
                            .text "POISON"

;; +----------------------------------+
;; |    ITEM DURABILITY LABELS        |
;; +----------------------------------+
itemdur_PRISTINE            .byte 8
                            .text "PRISTINE"
itemdur_SCRATCHED           .byte 9
                            .text "SCRATCHED"
itemdur_WORN                .byte 4
                            .text "WORN"
itemdur_DAMAGED             .byte 7
                            .text "DAMAGED"
itemdur_BROKEN              .byte 6
                            .text "BROKEN"
itemdur_BEYOND_REPAIR       .byte 13
                            .text "BEYOND REPAIR"

;; +----------------------------------+
;; |    ITEM DEFINITIONS              |
;; +----------------------------------+
itemNameLo:
                .byte <(itemname_SCROLL)              ;00 - Minor Healing
                .byte <(itemname_SCROLL)              ;01 - Medium Healing
                .byte <(itemname_SCROLL)              ;02 - Major Healing
                .byte <(itemname_SCROLL)              ;03 - Teleport
                .byte <(itemname_SCROLL)              ;04 - Identify
                .byte <(itemname_SCROLL)              ;05 - Unused
                .byte <(itemname_SCROLL)              ;06 - Unused
                .byte <(itemname_SCROLL)              ;07 - Unused
                .byte <(itemname_SCROLL)              ;08 - Unused
                .byte <(itemname_SCROLL)              ;09 - Unused
                .byte <(itemname_SCROLL)              ;0a - Unused
                .byte <(itemname_SCROLL)              ;0b - Unused
                .byte <(itemname_SCROLL)              ;0c - Unused
                .byte <(itemname_SCROLL)              ;0d - Unused
                .byte <(itemname_SCROLL)              ;0e - Unused
                .byte <(itemname_SCROLL)              ;0f - Unidentified Substitute - Scroll
                .byte <(itemname_POTION)              ;10 - Minor Healing
                .byte <(itemname_POTION)              ;11 - Medium Healing
                .byte <(itemname_POTION)              ;12 - Major Healing
                .byte <(itemname_POTION)              ;13 - Cure Poison
                .byte <(itemname_POTION)              ;14 - Clarity
                .byte <(itemname_POTION)              ;15 - Unused
                .byte <(itemname_POTION)              ;16 - Unused
                .byte <(itemname_POTION)              ;17 - Unused
                .byte <(itemname_POTION)              ;18 - Unused
                .byte <(itemname_POTION)              ;19 - Unidentified Substitute - Potion
                .byte <(itemname_PIECES_OF_GOLD)      ;1a - Pieces of Gold
                .byte $00                             ;1b - Unused
                .byte $00                             ;1c - Unused
                .byte $00                             ;1d - Unused
                .byte $00                             ;1e - Unused
                .byte $00                             ;1f - Unused
                .byte <(itemname_SHORT_SWORD)         ;20 - Short Sword
                .byte <(itemname_LONGSWORD)           ;21 - Longsword
                .byte $00                             ;22 - Unused
                .byte $00                             ;23 - Unused
                .byte $00                             ;24 - Unused
                .byte $00                             ;25 - Unused
                .byte $00                             ;26 - Unused
                .byte $00                             ;27 - Unused
                .byte $00                             ;28 - Unused
                .byte $00                             ;29 - Unused
                .byte $00                             ;2a - Unused
                .byte $00                             ;2b - Unused
                .byte $00                             ;2c - Unused
                .byte $00                             ;2d - Unused
                .byte $00                             ;2e - Unused
                .byte $00                             ;2f - Unused
                .byte <(itemname_LEATHER_ARMOR)       ;30 - Leather Armor
                .byte $00                             ;31 - Unused
                .byte $00                             ;32 - Unused
                .byte $00                             ;33 - Unused
                .byte $00                             ;34 - Unused
                .byte $00                             ;35 - Unused
                .byte $00                             ;36 - Unused
                .byte <(itemname_LEATHER_HELMET)      ;37 - Leather Helmet
                .byte <(itemname_IRON_HELMET)         ;38 - Iron Helmet
                .byte <(itemname_STEEL_HELMET)        ;39 - Steel Helmet
                .byte $00                             ;3a - Unused
                .byte $00                             ;3b - Unused
                .byte $00                             ;3c - Unused
                .byte $00                             ;3d - Unused
                .byte $00                             ;3e - Unused
                .byte $00                             ;3f - Unused
                .byte <(itemname_SMALL_SHIELD)        ;40 - Small Shield
                .byte <(itemname_LARGE_SHIELD)        ;41 - Small Shield


itemNameHi:
                .byte >(itemname_SCROLL)              ;00 - Minor Healing
                .byte >(itemname_SCROLL)              ;01 - Medium Healing
                .byte >(itemname_SCROLL)              ;02 - Major Healing
                .byte >(itemname_SCROLL)              ;03 - Teleport
                .byte >(itemname_SCROLL)              ;04 - Identify
                .byte >(itemname_SCROLL)              ;05 - Unused
                .byte >(itemname_SCROLL)              ;06 - Unused
                .byte >(itemname_SCROLL)              ;07 - Unused
                .byte >(itemname_SCROLL)              ;08 - Unused
                .byte >(itemname_SCROLL)              ;09 - Unused
                .byte >(itemname_SCROLL)              ;0a - Unused
                .byte >(itemname_SCROLL)              ;0b - Unused
                .byte >(itemname_SCROLL)              ;0c - Unused
                .byte >(itemname_SCROLL)              ;0d - Unused
                .byte >(itemname_SCROLL)              ;0e - Unused
                .byte >(itemname_SCROLL)              ;0f - Unidentified Substitute - Scroll
                .byte >(itemname_POTION)              ;10 - Minor Healing
                .byte >(itemname_POTION)              ;11 - Medium Healing
                .byte >(itemname_POTION)              ;12 - Major Healing
                .byte >(itemname_POTION)              ;13 - Cure Poison
                .byte >(itemname_POTION)              ;14 - Clarity
                .byte >(itemname_POTION)              ;15 - Unused
                .byte >(itemname_POTION)              ;16 - Unused
                .byte >(itemname_POTION)              ;17 - Unused
                .byte >(itemname_POTION)              ;18 - Unused
                .byte >(itemname_POTION)              ;19 - Unidentified Substitute - Potion
                .byte >(itemname_PIECES_OF_GOLD)      ;1a - Pieces of Gold
                .byte $00                             ;1b - Unused
                .byte $00                             ;1c - Unused
                .byte $00                             ;1d - Unused
                .byte $00                             ;1e - Unused
                .byte $00                             ;1f - Unused
                .byte >(itemname_SHORT_SWORD)         ;20 - Short Sword
                .byte >(itemname_LONGSWORD)           ;21 - Longsword
                .byte $00                             ;22 - Unused
                .byte $00                             ;23 - Unused
                .byte $00                             ;24 - Unused
                .byte $00                             ;25 - Unused
                .byte $00                             ;26 - Unused
                .byte $00                             ;27 - Unused
                .byte $00                             ;28 - Unused
                .byte $00                             ;29 - Unused
                .byte $00                             ;2a - Unused
                .byte $00                             ;2b - Unused
                .byte $00                             ;2c - Unused
                .byte $00                             ;2d - Unused
                .byte $00                             ;2e - Unused
                .byte $00                             ;2f - Unused
                .byte >(itemname_LEATHER_ARMOR)       ;30 - Leather Armor
                .byte $00                             ;31 - Unused
                .byte $00                             ;32 - Unused
                .byte $00                             ;33 - Unused
                .byte $00                             ;34 - Unused
                .byte $00                             ;35 - Unused
                .byte $00                             ;36 - Unused
                .byte >(itemname_LEATHER_HELMET)      ;37 - Leather Helmet
                .byte >(itemname_IRON_HELMET)         ;38 - Iron Helmet
                .byte >(itemname_STEEL_HELMET)        ;39 - Steel Helmet
                .byte $00                             ;3a - Unused
                .byte $00                             ;3b - Unused
                .byte $00                             ;3c - Unused
                .byte $00                             ;3d - Unused
                .byte $00                             ;3e - Unused
                .byte $00                             ;3f - Unused
                .byte >(itemname_SMALL_SHIELD)        ;40 - Small Shield
                .byte >(itemname_LARGE_SHIELD)        ;41 - Small Shield


itemValue       .byte $01                  ;00 - Actual Type - MINOR HEALING
                .byte $02                  ;01 - Actual Type - MEDIUM HEALING
                .byte $03                  ;02 - Actual Type - MAJOR HEALING
                .byte $04                  ;03 - Actual Type - TELEPORT
                .byte $06                  ;04 - Actual Type - IDENTIFY
                .byte $00                  ;05 - Unused
                .byte $00                  ;06 - Unused
                .byte $00                  ;07 - Unused
                .byte $00                  ;08 - Unused
                .byte $00                  ;09 - Unused
                .byte $00                  ;0a - Unused
                .byte $00                  ;0b - Unused
                .byte $00                  ;0c - Unused
                .byte $00                  ;0d - Unused
                .byte $00                  ;0e - Unused
                .byte $00                  ;0f - Unidentified Substitute - Scroll
                .byte $01                  ;10 - Actual Type - MINOR HEALING
                .byte $02                  ;11 - Actual Type - MEDIUM HEALING
                .byte $03                  ;12 - Actual Type - MAJOR HEALING
                .byte $05                  ;13 - Actual Type - CURE POISON
                .byte $07                  ;14 - Actual Type - CLARITY
                .byte $08                  ;15 - Actual Type - WATER
                .byte $09                  ;16 - Actual Type - POISON
                .byte $00                  ;17 - Unused
                .byte $00                  ;18 - Unused
                .byte $00                  ;19 - Unidentified Substitute - Potion
                .byte $00                  ;1a - N/A - Pieces of Gold
                .byte $00                  ;1b - Unused
                .byte $00                  ;1c - Unused
                .byte $00                  ;1d - Unused
                .byte $00                  ;1e - Unused
                .byte $00                  ;1f - Unused
                .byte $00                  ;20 - N/A - Shortsword
                .byte $00                  ;21 - N/A - Longsword
                .byte $00                  ;22 - Unused
                .byte $00                  ;23 - Unused
                .byte $00                  ;24 - Unused
                .byte $00                  ;25 - Unused
                .byte $00                  ;26 - Unused
                .byte $00                  ;27 - Unused
                .byte $00                  ;28 - Unused
                .byte $00                  ;29 - Unused
                .byte $00                  ;2a - Unused
                .byte $00                  ;2b - Unused
                .byte $00                  ;2c - Unused
                .byte $00                  ;2d - Unused
                .byte $00                  ;2e - Unused
                .byte $00                  ;2f - Unused
                .byte $00                  ;30 - N/A - Leather armor
                .byte $00                  ;31 - Unused
                .byte $00                  ;32 - Unused
                .byte $00                  ;33 - Unused
                .byte $00                  ;34 - Unused
                .byte $00                  ;35 - Unused
                .byte $00                  ;36 - Unused
                .byte $00                  ;37 - N/A - Leather helmet
                .byte $00                  ;38 - N/A - Iron helmet
                .byte $00                  ;39 - N/A - Steel helmet
                .byte $00                  ;3a - Unused
                .byte $00                  ;3b - Unused
                .byte $00                  ;3c - Unused
                .byte $00                  ;3d - Unused
                .byte $00                  ;3e - Unused
                .byte $00                  ;3f - Unused
                .byte $00                  ;40 - N/A - Small Shield
                .byte $00                  ;41 - N/A - Large Shield

itemTile        .byte $40                  ;00 - Scroll (yellow)
                .byte $40                  ;01 - Scroll (yellow)
                .byte $40                  ;02 - Scroll (yellow)
                .byte $40                  ;03 - Scroll (yellow)
                .byte $00                  ;04 - Scroll (yellow)
                .byte $00                  ;05 - Unused
                .byte $00                  ;06 - Unused
                .byte $00                  ;07 - Unused
                .byte $00                  ;08 - Unused
                .byte $00                  ;09 - Unused
                .byte $00                  ;0a - Unused
                .byte $00                  ;0b - Unused
                .byte $00                  ;0c - Unused
                .byte $00                  ;0d - Unused
                .byte $00                  ;0e - Unused
                .byte $40                  ;0f - Unidentified Substitute - Scroll
                .byte $45                  ;10 - Potion (Red)
                .byte $45                  ;11 - Potion (Red)
                .byte $45                  ;12 - Potion (Red)
                .byte $4c                  ;13 - Potion (Green)
                .byte $47                  ;14 - Potion (Purple)
                .byte $4b                  ;15 - Potion (Turqoise)
                .byte $46                  ;16 - Potion (Green)
                .byte $00                  ;17 - Unused
                .byte $00                  ;18 - Unused
                .byte $41                  ;19 - Unidentified Substitute - Potion
                .byte $42                  ;1a - Pieces of Gold
                .byte $00                  ;1b - Unused
                .byte $00                  ;1c - Unused
                .byte $00                  ;1d - Unused
                .byte $00                  ;1e - Unused
                .byte $00                  ;1f - Unused
                .byte $48                  ;20 - Short sword
                .byte $49                  ;21 - Longsword
                .byte $00                  ;22 - Unused
                .byte $00                  ;23 - Unused
                .byte $00                  ;24 - Unused
                .byte $00                  ;25 - Unused
                .byte $00                  ;26 - Unused
                .byte $00                  ;27 - Unused
                .byte $00                  ;28 - Unused
                .byte $00                  ;29 - Unused
                .byte $00                  ;2a - Unused
                .byte $00                  ;2b - Unused
                .byte $00                  ;2c - Unused
                .byte $00                  ;2d - Unused
                .byte $00                  ;2e - Unused
                .byte $00                  ;2f - Unused
                .byte $44                  ;30 - Leather armor
                .byte $00                  ;31 - Unused
                .byte $00                  ;32 - Unused
                .byte $00                  ;33 - Unused
                .byte $00                  ;34 - Unused
                .byte $00                  ;35 - Unused
                .byte $00                  ;36 - Unused
                .byte $4a                  ;37 - Leather Helmet
                .byte $4a                  ;38 - Iron Helmet
                .byte $4a                  ;39 - Steel Helmet
                .byte $00                  ;3a - Unused
                .byte $00                  ;3b - Unused
                .byte $00                  ;3c - Unused
                .byte $00                  ;3d - Unused
                .byte $00                  ;3e - Unused
                .byte $00                  ;3f - Unused
                .byte $4d                  ;40 - Small Shield
                .byte $4d                  ;41 - Large Shield

itemAttributes  .byte %10000001            ; 00 - Scroll - Heal Minor
                .byte %10000001            ; 01 - Scroll - Heal Medium
                .byte %10000001            ; 02 - Scroll - Heal Major
                .byte %10000001            ; 03 - Scroll - Teleport
                .byte %10000001            ; 04 - Scroll - Identify
                .byte %10000001            ; 05 - Unused
                .byte %10000001            ; 06 - Unused
                .byte %10000001            ; 07 - Unused
                .byte %10000001            ; 08 - Unused
                .byte %10000001            ; 09 - Unused
                .byte %10000001            ; 0a - Unused
                .byte %10000001            ; 0b - Unused
                .byte %10000001            ; 0c - Unused
                .byte %10000001            ; 0d - Unused
                .byte %10000001            ; 0e - Unused
                .byte %11000001            ; 0f - Scroll - Unidentifed substitute
                .byte %10000010            ; 10 - Potion - Heal Minor
                .byte %10000010            ; 11 - Potion - Heal Medium
                .byte %10000010            ; 12 - Potion - Heal Major
                .byte %10000010            ; 13 - Potion - Teleport
                .byte %10000010            ; 14 - Potion - Clarity
                .byte %10000010            ; 15 - Unused
                .byte %10000010            ; 16 - Unused
                .byte %10000010            ; 17 - Unused
                .byte %10000010            ; 18 - Unused
                .byte %11000010            ; 19 - Potion - Unidentifed substitute
                .byte %10000000            ; 1a - Pieces of Gold
                .byte %10000000            ; 1b - Unused
                .byte %10000000            ; 1c - Unused
                .byte %10000000            ; 1d - Unused
                .byte %10000000            ; 1e - Unused
                .byte %10000000            ; 1f - Unused
                .byte %10000110            ; 20 - Short sword
                .byte %10000110            ; 21 - Long sword
                .byte %10000000            ; 22 - Unused
                .byte %10000000            ; 23 - Unused
                .byte %10000000            ; 24 - Unused
                .byte %10000000            ; 25 - Unused
                .byte %10000000            ; 26 - Unused
                .byte %10000000            ; 27 - Unused
                .byte %10000000            ; 28 - Unused
                .byte %10000000            ; 29 - Unused
                .byte %10000000            ; 2a - Unused
                .byte %10000000            ; 2b - Unused
                .byte %10000000            ; 2c - Unused
                .byte %10000000            ; 2d - Unused
                .byte %10000000            ; 2e - Unused
                .byte %10000000            ; 2f - Unused
                .byte %10001100            ; 30 - Leather armor
                .byte %10000000            ; 31 - Unused
                .byte %10000000            ; 32 - Unused
                .byte %10000000            ; 33 - Unused
                .byte %10000000            ; 34 - Unused
                .byte %10000000            ; 35 - Unused
                .byte %10000000            ; 36 - Unused
                .byte %10001011            ; 37 - Leather Helmet
                .byte %10001011            ; 38 - Iron Helmet
                .byte %10001011            ; 39 - Steel Helmet
                .byte %10000000            ; 3a - Unused
                .byte %10000000            ; 3b - Unused
                .byte %10000000            ; 3c - Unused
                .byte %10000000            ; 3d - Unused             item
                .byte %10000000            ; 3e - Unused
                .byte %10000000            ; 3f - Unused
                .byte %10001010            ; 40 - Small shield
                .byte %10001010            ; 41 - Large shield

;; +----------------------------------+
;; |    ITEM SUBTYPE DEFINITIONS      |
;; +----------------------------------+
itemSubType_nameLo
                .byte <(itemtype_UNKNOWN)
                .byte <(itemtype_MINOR_HEALING)
                .byte <(itemtype_MEDIUM_HEALING)
                .byte <(itemtype_MAJOR_HEALING)
                .byte <(itemtype_TELEPORT)
                .byte <(itemtype_CURE_POISON)
                .byte <(itemtype_IDENTIFY)
                .byte <(itemtype_CLARITY)
                .byte <(itemtype_WATER)
                .byte <(itemtype_POISON)
itemSubType_nameHi
                .byte >(itemtype_UNKNOWN)
                .byte >(itemtype_MINOR_HEALING)
                .byte >(itemtype_MEDIUM_HEALING)
                .byte >(itemtype_MAJOR_HEALING)
                .byte >(itemtype_TELEPORT)
                .byte >(itemtype_CURE_POISON)
                .byte >(itemtype_IDENTIFY)
                .byte >(itemtype_CLARITY)
                .byte >(itemtype_WATER)
                .byte >(itemtype_POISON)
itemSubType_color
                .byte $06
                .byte $02
                .byte $02
                .byte $02
                .byte $04
                .byte $07
                .byte $04
                .byte $04
                .byte $03
                .byte $05

;; +----------------------------------+
;; |    ITEM SUBTYPE DEFINITIONS      |
;; +----------------------------------+
itemDurability_nameLo:
                .byte <(itemdur_PRISTINE)
                .byte <(itemdur_SCRATCHED)
                .byte <(itemdur_WORN)
                .byte <(itemdur_DAMAGED)
                .byte <(itemdur_BROKEN)
                .byte <(itemdur_BEYOND_REPAIR)

itemDurability_nameHi:
                .byte >(itemdur_PRISTINE)
                .byte >(itemdur_SCRATCHED)
                .byte >(itemdur_WORN)
                .byte >(itemdur_DAMAGED)
                .byte >(itemdur_BROKEN)
                .byte >(itemdur_BEYOND_REPAIR)

itemDurability_color:
                .byte $05
                .byte $03
                .byte $07
                .byte $02
                .byte $02
                .byte $06

itemDurability_threshold:
                .byte $c0
                .byte $a0
                .byte $60
                .byte $40
                .byte $10
                .byte $00

;; +----------------------------------+
;; |    ITEM SET FOR RANDOMIZATION    |
;; +----------------------------------+
; Tables for random selection/generation of items
itemSet_size    .byte $0c
itemSet_goldRnd .byte $0c
itemSet_goldMod .byte $02
itemSet_type:
                .byte $30 ; - Leather Armor
                .byte $21 ; - Long Sword
                .byte $40 ; - Small shield
                .byte $37 ; - Leather Helmet
                .byte $20 ; - Short Sword
                .byte $00 ; - Scroll of Heal Minor
                .byte $03 ; - Scroll of Teleport
                .byte $13 ; - Potion of Cure Poison
                .byte $10 ; - Potion of Heal Minor
                .byte $15 ; - Poison
                .byte $16 ; - Water
                .byte $1a ; - Pieces of Gold

itemSet_weight:
                .byte $01 ; - Leather Armor
                .byte $01 ; - Long Sword
                .byte $02 ; - Small Shield
                .byte $02 ; - Leather Helmet
                .byte $02 ; - Short Sword
                .byte $03 ; - Scroll of Heal Minor
                .byte $03 ; - Scroll of Teleport
                .byte $03 ; - Potion of Cure Poison
                .byte $03 ; - Potion of Heal Minor
                .byte $03 ; - Poison
                .byte $03 ; - Water
                .byte $10 ; - Pieces of Gold
