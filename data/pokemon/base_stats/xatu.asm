	db XATU ; 178

	db  65,  75,  70,  95,  95,  70
	;   hp  atk  def  spd  sat  sdf

	db PSYCHIC_TYPE, FLYING ; type
	db 75 ; catch rate
	db 171 ; base exp
	db NO_ITEM, NO_ITEM ; items
	db GENDER_F50 ; gender ratio
	db 100 ; unknown 1
	db 20 ; step cycles to hatch
	db 5 ; unknown 2
	INCBIN "gfx/pokemon/xatu/front.dimensions"
	dw NULL, NULL ; unused (beta front/back pics)
	db GROWTH_MEDIUM_FAST ; growth rate
	dn EGG_FLYING, EGG_FLYING ; egg groups

	; tm/hm learnset
	tmhm CALM_MIND, TOXIC, HIDDEN_POWER, SUNNY_DAY, HYPER_BEAM, LIGHT_SCREEN, PROTECT, RAIN_DANCE, ROOST, SOLARBEAM, RETURN, PSYCHIC_M, SHADOW_BALL, REFLECT, REST, ATTRACT, THIEF, STEEL_WING, THUNDER_WAVE, FLY, SLEEP_TALK, U_TURN, SUBSTITUTE, DAZLINGGLEAM, FOUL_PLAY, GIGA_DRAIN, HEAT_WAVE, PAIN_SPLIT, SIGNAL_BEAM, ZEN_HEADBUTT, GRASS_KNOT
	; end
