	db SUDOWOODO ; 185

	db  70, 100, 115,  30,  30,  65
	;   hp  atk  def  spd  sat  sdf

	db ROCK, ROCK ; type
	db 65 ; catch rate
	db 135 ; base exp
	db NO_ITEM, NO_ITEM ; items
	db GENDER_F50 ; gender ratio
	db 100 ; unknown 1
	db 20 ; step cycles to hatch
	db 5 ; unknown 2
	INCBIN "gfx/pokemon/sudowoodo/front.dimensions"
	dw NULL, NULL ; unused (beta front/back pics)
	db GROWTH_MEDIUM_FAST ; growth rate
	dn EGG_MINERAL, EGG_MINERAL ; egg groups

	; tm/hm learnset
	tmhm CALM_MIND, TOXIC, HIDDEN_POWER, SUNNY_DAY, PROTECT, EARTHQUAKE, RETURN, BRICK_BREAK, REST, ATTRACT, THIEF, EXPLOSION, ROCK_POLISH, STONE_EDGE, BULLDOZE, ROCK_SLIDE, SLEEP_TALK, SUBSTITUTE, EARTH_POWER, FIRE_PUNCH, FOUL_PLAY, ICE_PUNCH, LOW_KICK, STEALTH_ROCK, THUNDERPUNCH
	; end
