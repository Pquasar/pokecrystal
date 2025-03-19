	db MAGCARGO ; 219

	db  60,  50, 120,  30,  90,  80
	;   hp  atk  def  spd  sat  sdf

	db FIRE, ROCK ; type
	db 75 ; catch rate
	db 154 ; base exp
	db NO_ITEM, NO_ITEM ; items
	db GENDER_F50 ; gender ratio
	db 100 ; unknown 1
	db 20 ; step cycles to hatch
	db 5 ; unknown 2
	INCBIN "gfx/pokemon/magcargo/front.dimensions"
	dw NULL, NULL ; unused (beta front/back pics)
	db GROWTH_MEDIUM_FAST ; growth rate
	dn EGG_INDETERMINATE, EGG_INDETERMINATE ; egg groups

	; tm/hm learnset
	tmhm TOXIC, HIDDEN_POWER, SUNNY_DAY, HYPER_BEAM, LIGHT_SCREEN, PROTECT, SOLARBEAM, EARTHQUAKE, RETURN, REFLECT, FLAMETHROWER, SANDSTORM, FIRE_BLAST, REST, ATTRACT, OVERHEAT, WILL_O_WISP, EXPLOSION, ROCK_POLISH, STONE_EDGE, GYRO_BALL, BULLDOZE, ROCK_SLIDE, INFESTATION, SLEEP_TALK, SUBSTITUTE, EARTH_POWER, HEAT_WAVE, IRON_DEFENSE, PAIN_SPLIT, STEALTH_ROCK
	; end
