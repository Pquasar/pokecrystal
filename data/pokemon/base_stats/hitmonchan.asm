	db HITMONCHAN ; 107

	db  50, 105,  79,  76,  35, 110
	;   hp  atk  def  spd  sat  sdf

	db FIGHTING, FIGHTING ; type
	db 45 ; catch rate
	db 140 ; base exp
	db NO_ITEM, NO_ITEM ; items
	db GENDER_F0 ; gender ratio
	db 100 ; unknown 1
	db 25 ; step cycles to hatch
	db 5 ; unknown 2
	INCBIN "gfx/pokemon/hitmonchan/front.dimensions"
	dw NULL, NULL ; unused (beta front/back pics)
	db GROWTH_MEDIUM_FAST ; growth rate
	dn EGG_HUMANSHAPE, EGG_HUMANSHAPE ; egg groups

	; tm/hm learnset
	tmhm TOXIC, BULK_UP, HIDDEN_POWER, SUNNY_DAY, PROTECT, RAIN_DANCE, EARTHQUAKE, RETURN, BRICK_BREAK, REST, ATTRACT, THIEF, FOCUS_BLAST, STONE_EDGE, BULLDOZE, ROCK_SLIDE, SLEEP_TALK, SUBSTITUTE, DRAIN_PUNCH, FIRE_PUNCH, ICE_PUNCH, LOW_KICK, THROAT_CHOP, THUNDERPUNCH
	; end
