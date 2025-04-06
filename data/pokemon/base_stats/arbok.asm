	db ARBOK ; 024

	db  60,  95,  69,  80,  65,  79
	;   hp  atk  def  spd  sat  sdf

	db POISON, POISON ; type
	db 90 ; catch rate
	db 147 ; base exp
	db NO_ITEM, NO_ITEM ; items
	db GENDER_F50 ; gender ratio
	db 100 ; unknown 1
	db 20 ; step cycles to hatch
	db 5 ; unknown 2
	INCBIN "gfx/pokemon/arbok/front.dimensions"
	dw NULL, NULL ; unused (beta front/back pics)
	db GROWTH_MEDIUM_FAST ; growth rate
	dn EGG_GROUND, EGG_DRAGON ; egg groups

	; tm/hm learnset
	tmhm TOXIC, HIDDEN_POWER, SUNNY_DAY, HYPER_BEAM, PROTECT, RAIN_DANCE, EARTHQUAKE, RETURN, SLUDGE_BOMB, REST, ATTRACT, THIEF, BULLDOZE, ROCK_SLIDE, DRAGON_TAIL, INFESTATION, POISON_JAB, SLEEP_TALK, SUBSTITUTE, DARK_PULSE, AQUA_TAIL, GIGA_DRAIN, GUNK_SHOT, IRON_TAIL, SEED_BOMB, THROAT_CHOP
	; end
