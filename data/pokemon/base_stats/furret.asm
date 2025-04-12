	db FURRET ; 162

	db  85,  76,  64,  90,  45,  55
	;   hp  atk  def  spd  sat  sdf

	db NORMAL, NORMAL ; type
	db 90 ; catch rate
	db 116 ; base exp
	db BERRY, BERRY ; items
	db GENDER_F50 ; gender ratio
	db 100 ; unknown 1
	db 15 ; step cycles to hatch
	db 5 ; unknown 2
	INCBIN "gfx/pokemon/furret/front.dimensions"
	dw NULL, NULL ; unused (beta front/back pics)
	db GROWTH_MEDIUM_FAST ; growth rate
	dn EGG_GROUND, EGG_GROUND ; egg groups

	; tm/hm learnset
	tmhm TOXIC, HIDDEN_POWER, SUNNY_DAY, ICE_BEAM, BLIZZARD, HYPER_BEAM, PROTECT, RAIN_DANCE, SOLARBEAM, THUNDERBOLT, THUNDER, RETURN, SHADOW_BALL, BRICK_BREAK, FLAMETHROWER, REST, ATTRACT, THIEF, FOCUS_BLAST, SLEEP_TALK, U_TURN, SUBSTITUTE, SURF, AQUA_TAIL, FIRE_PUNCH, HYPER_VOICE, ICE_PUNCH, IRON_TAIL, KNOCK_OFF, THUNDERPUNCH, GRASS_KNOT, SHADOW_CLAW
	; end
