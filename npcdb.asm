
npcname_GIANT_RAT           .byte 09
                            .text "GIANT RAT"
npcname_SKELETON_WARRIOR    .byte 16
                            .text "SKELETON WARRIOR"
npcname_KOBOLD              .byte 6
                            .text "KOBOLD"
npcname_ORC                 .byte 3
                            .text "ORC"
npcname_GIANT_SPIDER        .byte 12
                            .text "GIANT SPIDER"
npcname_VIPER               .byte 5
                            .text "VIPER"


npc_nameLo:
                .byte <(npcname_GIANT_RAT)                      ; 00 - Giant Rat
                .byte <(npcname_SKELETON_WARRIOR)               ; 01 - Skeleton Warrior
                .byte <(npcname_KOBOLD)                         ; 02 - Kobold
                .byte <(npcname_ORC)                            ; 03 - Orc
                .byte <(npcname_GIANT_SPIDER)                   ; 04 - Giant Spider
                .byte <(npcname_VIPER)                          ; 05 - Viper
            
npc_nameHi:
                .byte >(npcname_GIANT_RAT)                      ; 00 - Giant Rat
                .byte >(npcname_SKELETON_WARRIOR)               ; 01 - Skeleton Warrior
                .byte >(npcname_KOBOLD)                         ; 02 - Kobold
                .byte >(npcname_ORC)                            ; 03 - Orc
                .byte >(npcname_GIANT_SPIDER)                   ; 04 - Giant Spider
                .byte >(npcname_VIPER)                          ; 05 - Viper

npc_maxHP:      .byte $0c                                       ; 00 - Giant Rat
                .byte $12                                       ; 01 - Skeleton Warrior
                .byte $06                                       ; 02 - Kobold
                .byte $20                                       ; 03 - Orc
                .byte $12                                       ; 04 - Giant Spider
                .byte $06                                       ; 05 - Viper
            
npc_moveCost:
                .byte $0f                                       ; 00 - Giant Rat
                .byte $14                                       ; 01 - Skeleton Warrior
                .byte $0b                                       ; 02 - Kobold
                .byte $0d                                       ; 03 - Orc
                .byte $0c                                       ; 04 - Giant Spider
                .byte $0d                                       ; 05 - Viper

npc_tileID:
                .byte $30                                       ; 00 - Giant Rat
                .byte $31                                       ; 01 - Skeleton Warrior
                .byte $32                                       ; 02 - Kobold
                .byte $33                                       ; 03 - Orc
                .byte $34                                       ; 04 - Giant Spider
                .byte $35                                       ; 05 - Viper

npc_spritePtr:
                .byte $89                                       ; 00 - Giant Rat
                .byte $8b                                       ; 01 - Skeleton Warrior
                .byte $8d                                       ; 02 - Kobold
                .byte $8f                                       ; 03 - Orc
                .byte $93                                       ; 04 - Giant Spider
                .byte $91                                       ; 05 - Viper

npc_attributes:
                .byte %10000000                                 ; 00 - Giant Rat
                .byte %10000000                                 ; 01 - Skeleton Warrior
                .byte %10000000                                 ; 02 - Kobold
                .byte %10000000                                 ; 03 - Orc
                .byte %10000000                                 ; 02 - Giant Spider
                .byte %10000000                                 ; 03 - Viper


; Tables for random selection/generation of items
npcSet_size     .byte $0c
npcSet_type:
                .byte $00 ; - Giant Rat
                .byte $01 ; - Skeleton Warrior
                .byte $02 ; - Kobold
                .byte $03 ; - Orc
                .byte $04 ; - Giant Spider
                .byte $05 ; - Viper

npcSet_weight:
                .byte $08 ; - Giant Rat
                .byte $04 ; - Skeleton Warrior
                .byte $08 ; - Kobold
                .byte $02 ; - Orc
                .byte $06 ; - Giant Spider
                .byte $04 ; - Viper