	db CORSOLA ; 222

	db  65,  55,  95,  35,  65,  95
	;   hp  atk  def  spd  sat  sdf

	db WATER, ROCK ; type
	db 60 ; catch rate
	db 113 ; base exp
	db NO_ITEM, NO_ITEM ; items
	db GENDER_F75 ; gender ratio
	db 100 ; unknown 1
	db 20 ; step cycles to hatch
	db 5 ; unknown 2
	INCBIN "gfx/pokemon/corsola/front.dimensions"
	dw NULL, NULL ; unused (beta front/back pics)
	db GROWTH_FAST ; growth rate
	dn EGG_WATER_1, EGG_WATER_3 ; egg groups

	; tm/hm learnset
	tmhm CALM_MIND, TOXIC, HAIL, HIDDEN_POWER, SUNNY_DAY, ICE_BEAM, BLIZZARD, LIGHT_SCREEN, PROTECT, RAIN_DANCE, SAFEGUARD, EARTHQUAKE, RETURN, PSYCHIC_M, SHADOW_BALL, REFLECT, SANDSTORM, REST, ATTRACT, SCALD, EXPLOSION, ROCK_POLISH, STONE_EDGE, BULLDOZE, ROCK_SLIDE, SLEEP_TALK, SUBSTITUTE, EARTH_POWER, ICY_WIND, IRON_DEFENSE, LIQUIDATION, THROAT_CHOP, STEALTH_ROCK, SURF
	; end
