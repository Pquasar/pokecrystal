	db WIGGLYTUFF ; 040

	db 140,  70,  45,  45,  85,  50
	;   hp  atk  def  spd  sat  sdf

	db NORMAL, FAIRY ; type
	db 50 ; catch rate
	db 109 ; base exp
	db NO_ITEM, NO_ITEM ; items
	db GENDER_F75 ; gender ratio
	db 100 ; unknown 1
	db 10 ; step cycles to hatch
	db 5 ; unknown 2
	INCBIN "gfx/pokemon/wigglytuff/front.dimensions"
	dw NULL, NULL ; unused (beta front/back pics)
	db GROWTH_FAST ; growth rate
	dn EGG_FAIRY, EGG_FAIRY ; egg groups

	; tm/hm learnset
	tmhm TOXIC, HIDDEN_POWER, SUNNY_DAY, ICE_BEAM, BLIZZARD, HYPER_BEAM, LIGHT_SCREEN, PROTECT, RAIN_DANCE, SOLARBEAM, THUNDERBOLT, THUNDER, RETURN, PSYCHIC_M, SHADOW_BALL, BRICK_BREAK, REFLECT, FLAMETHROWER, FIRE_BLAST, REST, ATTRACT, FOCUS_BLAST, THUNDER_WAVE, GYRO_BALL, SLEEP_TALK, SUBSTITUTE, WILD_CHARGE, DAZLINGGLEAM, DRAIN_PUNCH, FIRE_PUNCH, HEAL_BELL, HYPER_VOICE, ICE_PUNCH, ICY_WIND, KNOCK_OFF, PAIN_SPLIT, STEALTH_ROCK, THUNDERPUNCH, GRASS_KNOT
	; end
