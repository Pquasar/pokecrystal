	db BEEDRILL ; 015

	db  65,  90,  40,  75,  45,  80
	;   hp  atk  def  spd  sat  sdf

	db BUG, POISON ; type
	db 45 ; catch rate
	db 159 ; base exp
	db NO_ITEM, POISON_BARB ; items
	db GENDER_F50 ; gender ratio
	db 100 ; unknown 1
	db 15 ; step cycles to hatch
	db 5 ; unknown 2
	INCBIN "gfx/pokemon/beedrill/front.dimensions"
	dw NULL, NULL ; unused (beta front/back pics)
	db GROWTH_MEDIUM_FAST ; growth rate
	dn EGG_BUG, EGG_BUG ; egg groups

	; tm/hm learnset
	tmhm TOXIC, HIDDEN_POWER, SUNNY_DAY, HYPER_BEAM, PROTECT, ROOST, SOLARBEAM, RETURN, BRICK_BREAK, SLUDGE_BOMB, REST, ATTRACT, THIEF, ACROBATICS, SWORDS_DANCE, X_SCISSOR, INFESTATION, POISON_JAB, SLEEP_TALK, U_TURN, SUBSTITUTE, DRILL_RUN, GIGA_DRAIN, KNOCK_OFF, THROAT_CHOP, CUT
	; end
