Marts:
; entries correspond to MART_* constants (see constants/mart_constants.asm)
	table_width 2
	dw MartCherrygrove
	dw MartCherrygroveDex
	dw MartViolet
	dw MartAzalea
	dw MartCianwood
	dw MartGoldenrod2F1
	dw MartGoldenrod2F2
	dw MartGoldenrod3F
	dw MartGoldenrod4F
	dw MartGoldenrod5F1
	dw MartGoldenrod5F2
	dw MartGoldenrod5F3
	dw MartGoldenrod5F4
	dw MartOlivine
	dw MartEcruteak
	dw MartMahogany1
	dw MartMahogany2
	dw MartBlackthorn
	dw MartViridian
	dw MartPewter
	dw MartCerulean
	dw MartLavender
	dw MartVermilion
	dw MartCeladon2F1
	dw MartCeladon2F2
	dw MartCeladon3F
	dw MartCeladon4F
	dw MartCeladon5F1
	dw MartCeladon5F2
	dw MartFuchsia
	dw MartSaffron
	dw MartMtMoon
	dw MartIndigoPlateau
	dw MartUnderground
	assert_table_length NUM_MARTS

MartCherrygrove:
	db 10 ; # items
	db LEFTOVERS
	db GOLD_BERRY
	db MIRACLEBERRY
	db HARD_STONE
	db BLACKGLASSES
	db DRAGON_FANG
	db POLKADOT_BOW
	db METAL_COAT
	db MIRACLE_SEED
	db PINK_BOW
	db -1 ; end

MartCherrygroveDex:
	db 5 ; # items
	db POKE_BALL
	db POTION
	db ANTIDOTE
	db PARLYZ_HEAL
	db AWAKENING
	db -1 ; end

MartViolet:
	db 10 ; # items
	db LEFTOVERS
	db GOLD_BERRY
	db MIRACLEBERRY
	db SILVERPOWDER
	db SHARP_BEAK
	db POISON_BARB
	db MYSTIC_WATER
	db BLACKBELT_I
	db CHARCOAL
	db TWISTEDSPOON
	db -1 ; end

MartAzalea:
	db 10 ; # items
	db LEFTOVERS
	db GOLD_BERRY
	db MIRACLEBERRY
	db MASTER_BALL
	db SHARP_BEAK
	db MAGNET
	db POLKADOT_BOW
	db METAL_COAT
	db SOFT_SAND
	db SPELL_TAG
	db -1 ; end

MartCianwood:
	db 5 ; # items
	db POTION
	db SUPER_POTION
	db HYPER_POTION
	db FULL_HEAL
	db REVIVE
	db -1 ; end

MartGoldenrod2F1:
	db 10 ; # items
	db LEFTOVERS
	db GOLD_BERRY
	db MIRACLEBERRY
	db MAX_REVIVE
	db BLACKBELT_I
	db CHARCOAL
	db MYSTIC_WATER
	db PINK_BOW
	db CHARCOAL
	db TWISTEDSPOON
	db -1 ; end

MartGoldenrod2F2:
	db 10 ; # items
	db LEFTOVERS
	db GOLD_BERRY
	db MIRACLEBERRY
	db MAX_REPEL
	db SHARP_BEAK
	db POISON_BARB
	db MYSTIC_WATER
	db BLACKBELT_I
	db CHARCOAL
	db MAGNET
	db -1 ; end

MartGoldenrod3F:
	db 3 ; # items
	db FULL_RESTORE
	db MAX_REVIVE
	db MAX_REPEL
	db -1 ; end

MartGoldenrod4F:
	db 1 ; # items
	db HP_UP
	db -1 ; end

MartGoldenrod5F1:
	db 3 ; # items
	db TM_EXPLOSION
	db TM_EXPLOSION
	db TM_EXPLOSION
	db -1 ; end

MartGoldenrod5F2:
	db 4 ; # items
	db TM_EXPLOSION
	db TM_EXPLOSION
	db TM_EXPLOSION
	db TM_LIGHT_SCREEN
	db -1 ; end

MartGoldenrod5F3:
	db 4 ; # items
	db TM_EXPLOSION
	db TM_EXPLOSION
	db TM_EXPLOSION
	db TM_LIGHT_SCREEN
	db -1 ; end

MartGoldenrod5F4:
	db 5 ; # items
	db TM_EXPLOSION
	db TM_EXPLOSION
	db TM_EXPLOSION
	db TM_LIGHT_SCREEN
	db TM_LIGHT_SCREEN
	db -1 ; end

MartOlivine:
	db 9 ; # items
	db GREAT_BALL
	db SUPER_POTION
	db HYPER_POTION
	db ANTIDOTE
	db PARLYZ_HEAL
	db AWAKENING
	db ICE_HEAL
	db SUPER_REPEL
	db SURF_MAIL
	db -1 ; end

MartEcruteak:
	db 10 ; # items
	db LEFTOVERS
	db GOLD_BERRY
	db MIRACLEBERRY
	db MAX_REVIVE
	db MIRACLE_SEED
	db CHARCOAL
	db NEVERMELTICE
	db POLKADOT_BOW
	db CHARCOAL
	db BLACKGLASSES
	db -1 ; end

MartMahogany1:
	db 4 ; # items
	db TINYMUSHROOM
	db SLOWPOKETAIL
	db POKE_BALL
	db POTION
	db -1 ; end

MartMahogany2:
	db 9 ; # items
	db RAGECANDYBAR
	db GREAT_BALL
	db SUPER_POTION
	db HYPER_POTION
	db ANTIDOTE
	db PARLYZ_HEAL
	db SUPER_REPEL
	db REVIVE
	db FLOWER_MAIL
	db -1 ; end

MartBlackthorn:
	db 9 ; # items
	db GREAT_BALL
	db ULTRA_BALL
	db HYPER_POTION
	db MAX_POTION
	db FULL_HEAL
	db REVIVE
	db MAX_REPEL
	db X_DEFEND
	db X_ATTACK
	db -1 ; end

MartViridian:
	db 9 ; # items
	db ULTRA_BALL
	db HYPER_POTION
	db FULL_HEAL
	db REVIVE
	db ANTIDOTE
	db PARLYZ_HEAL
	db AWAKENING
	db BURN_HEAL
	db FLOWER_MAIL
	db -1 ; end

MartPewter:
	db 7 ; # items
	db GREAT_BALL
	db SUPER_POTION
	db SUPER_REPEL
	db ANTIDOTE
	db PARLYZ_HEAL
	db AWAKENING
	db BURN_HEAL
	db -1 ; end

MartCerulean:
	db 9 ; # items
	db GREAT_BALL
	db ULTRA_BALL
	db SUPER_POTION
	db SUPER_REPEL
	db FULL_HEAL
	db X_DEFEND
	db X_ATTACK
	db DIRE_HIT
	db SURF_MAIL
	db -1 ; end

MartLavender:
	db 8 ; # items
	db GREAT_BALL
	db POTION
	db SUPER_POTION
	db MAX_REPEL
	db ANTIDOTE
	db PARLYZ_HEAL
	db AWAKENING
	db BURN_HEAL
	db -1 ; end

MartVermilion:
	db 8 ; # items
	db ULTRA_BALL
	db SUPER_POTION
	db HYPER_POTION
	db REVIVE
	db PARLYZ_HEAL
	db AWAKENING
	db BURN_HEAL
	db LITEBLUEMAIL
	db -1 ; end

MartCeladon2F1:
	db 7 ; # items
	db POTION
	db SUPER_POTION
	db HYPER_POTION
	db MAX_POTION
	db REVIVE
	db SUPER_REPEL
	db MAX_REPEL
	db -1 ; end

MartCeladon2F2:
	db 10 ; # items
	db POKE_BALL
	db GREAT_BALL
	db ULTRA_BALL
	db ESCAPE_ROPE
	db FULL_HEAL
	db ANTIDOTE
	db BURN_HEAL
	db ICE_HEAL
	db AWAKENING
	db PARLYZ_HEAL
	db -1 ; end

MartCeladon3F:
	db 5 ; # items
	db TM_HIDDEN_POWER
	db TM_SUNNY_DAY
	db TM_PROTECT
	db TM_RAIN_DANCE
	db TM_SANDSTORM
	db -1 ; end

MartCeladon4F:
	db 3 ; # items
	db POKE_DOLL
	db SURF_MAIL
	db -1 ; end

MartCeladon5F1:
	db 5 ; # items
	db HP_UP
	db PROTEIN
	db IRON
	db CARBOS
	db CALCIUM
	db -1 ; end

MartCeladon5F2:
	db 7 ; # items
	db X_ACCURACY
	db GUARD_SPEC
	db DIRE_HIT
	db X_ATTACK
	db X_DEFEND
	db X_SPEED
	db X_SPECIAL
	db -1 ; end

MartFuchsia:
	db 7 ; # items
	db GREAT_BALL
	db ULTRA_BALL
	db SUPER_POTION
	db HYPER_POTION
	db FULL_HEAL
	db MAX_REPEL
	db FLOWER_MAIL
	db -1 ; end

MartSaffron:
	db 8 ; # items
	db GREAT_BALL
	db ULTRA_BALL
	db HYPER_POTION
	db MAX_POTION
	db FULL_HEAL
	db X_ATTACK
	db X_DEFEND
	db FLOWER_MAIL
	db -1 ; end

MartMtMoon:
	db 6 ; # items
	db POKE_DOLL
	db FRESH_WATER
	db SODA_POP
	db LEMONADE
	db REPEL
	db PORTRAITMAIL
	db -1 ; end

MartIndigoPlateau:
	db 7 ; # items
	db ULTRA_BALL
	db MAX_REPEL
	db HYPER_POTION
	db MAX_POTION
	db FULL_RESTORE
	db REVIVE
	db FULL_HEAL
	db -1 ; end

MartUnderground:
	db 4 ; # items
	db ENERGYPOWDER
	db ENERGY_ROOT
	db HEAL_POWDER
	db REVIVAL_HERB
	db -1 ; end

DefaultMart:
	db 2 ; # items
	db POKE_BALL
	db POTION
	db -1 ; end
