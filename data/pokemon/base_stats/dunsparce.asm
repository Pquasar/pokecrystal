	db DUNSPARCE ; 206

	db 100,  70,  70,  45,  65,  65
	;   hp  atk  def  spd  sat  sdf

	db NORMAL, NORMAL ; type
	db 190 ; catch rate
	db 75 ; base exp
	db NO_ITEM, NO_ITEM ; items
	db GENDER_F50 ; gender ratio
	db 100 ; unknown 1
	db 20 ; step cycles to hatch
	db 5 ; unknown 2
	INCBIN "gfx/pokemon/dunsparce/front.dimensions"
	dw NULL, NULL ; unused (beta front/back pics)
	db GROWTH_MEDIUM_FAST ; growth rate
	dn EGG_GROUND, EGG_GROUND ; egg groups

	; tm/hm learnset
	tmhm CALM_MIND, TOXIC, HIDDEN_POWER, SUNNY_DAY, ICE_BEAM, BLIZZARD, PROTECT, RAIN_DANCE, ROOST, SOLARBEAM, THUNDERBOLT, THUNDER, EARTHQUAKE, RETURN, SHADOW_BALL, FLAMETHROWER, FIRE_BLAST, REST, ATTRACT, THIEF, THUNDER_WAVE, GYRO_BALL, BULLDOZE, ROCK_SLIDE, POISON_JAB, SLEEP_TALK, SUBSTITUTE, WILD_CHARGE, AQUA_TAIL, DRILL_RUN, IRON_TAIL, PAIN_SPLIT, STEALTH_ROCK, ZEN_HEADBUTT
	; end
