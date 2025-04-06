	db LICKILICKY ; 048

	db 110,  85,  95,  50,  80,  95
	;   hp  atk  def  spd  sat  sdf

	db NORMAL, NORMAL ; type
	db 30 ; catch rate
	db 193 ; base exp
	db NO_ITEM, NO_ITEM ; items
	db GENDER_F50 ; gender ratio
	db 100 ; unknown 1
	db 20 ; step cycles to hatch
	db 5 ; unknown 2
	INCBIN "gfx/pokemon/lickilicky/front.dimensions"
	dw NULL, NULL ; unused (beta front/back pics)
	db GROWTH_MEDIUM_FAST ; growth rate
	dn EGG_MONSTER, EGG_MONSTER ; egg groups

	; tm/hm learnset
	tmhm TOXIC, HIDDEN_POWER, SUNNY_DAY, ICE_BEAM, BLIZZARD, HYPER_BEAM, PROTECT, RAIN_DANCE, SOLARBEAM, THUNDERBOLT, THUNDER, EARTHQUAKE, RETURN, SHADOW_BALL, BRICK_BREAK, FLAMETHROWER, SANDSTORM, FIRE_BLAST, REST, ATTRACT, THIEF, FOCUS_BLAST, EXPLOSION, GYRO_BALL, SWORDS_DANCE, BULLDOZE, ROCK_SLIDE, DRAGON_TAIL, SLEEP_TALK, SUBSTITUTE, SURF, AQUA_TAIL, FIRE_PUNCH, ICE_PUNCH, ICY_WIND, IRON_TAIL, KNOCK_OFF, THUNDERPUNCH, ZEN_HEADBUTT
	; end
