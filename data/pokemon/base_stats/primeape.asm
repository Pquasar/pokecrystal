	db PRIMEAPE ; 057

	db  65, 105,  60,  95,  60,  70
	;   hp  atk  def  spd  sat  sdf

	db FIGHTING, FIGHTING ; type
	db 75 ; catch rate
	db 149 ; base exp
	db NO_ITEM, NO_ITEM ; items
	db GENDER_F50 ; gender ratio
	db 100 ; unknown 1
	db 20 ; step cycles to hatch
	db 5 ; unknown 2
	INCBIN "gfx/pokemon/primeape/front.dimensions"
	dw NULL, NULL ; unused (beta front/back pics)
	db GROWTH_MEDIUM_FAST ; growth rate
	dn EGG_GROUND, EGG_GROUND ; egg groups

	; tm/hm learnset
	tmhm TOXIC, BULK_UP, HIDDEN_POWER, SUNNY_DAY, HYPER_BEAM, PROTECT, RAIN_DANCE, THUNDERBOLT, THUNDER, EARTHQUAKE, RETURN, BRICK_BREAK, REST, ATTRACT, THIEF, OVERHEAT, FOCUS_BLAST, ACROBATICS, STONE_EDGE, BULLDOZE, ROCK_SLIDE, POISON_JAB, SLEEP_TALK, U_TURN, SUBSTITUTE, DUAL_CHOP, FIRE_PUNCH, GUNK_SHOT, ICE_PUNCH, IRON_TAIL, LOW_KICK, OUTRAGE, SEED_BOMB, THROAT_CHOP, THUNDERPUNCH, STRENGTH, SHADOW_CLAW
	; end
