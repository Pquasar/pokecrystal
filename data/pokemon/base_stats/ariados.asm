	db ARIADOS ; 168

	db  70,  90,  70,  40,  60,  70
	;   hp  atk  def  spd  sat  sdf

	db BUG, POISON ; type
	db 90 ; catch rate
	db 134 ; base exp
	db NO_ITEM, NO_ITEM ; items
	db GENDER_F50 ; gender ratio
	db 100 ; unknown 1
	db 15 ; step cycles to hatch
	db 5 ; unknown 2
	INCBIN "gfx/pokemon/ariados/front.dimensions"
	dw NULL, NULL ; unused (beta front/back pics)
	db GROWTH_FAST ; growth rate
	dn EGG_BUG, EGG_BUG ; egg groups

	; tm/hm learnset
	tmhm TOXIC, HIDDEN_POWER, SUNNY_DAY, HYPER_BEAM, PROTECT, SOLARBEAM, RETURN, LEECH_LIFE, PSYCHIC_M, SLUDGE_BOMB, REST, ATTRACT, THIEF, SWORDS_DANCE, X_SCISSOR, INFESTATION, POISON_JAB, SLEEP_TALK, SUBSTITUTE, GIGA_DRAIN, SIGNAL_BEAM, THROAT_CHOP, FOUL_PLAY
	; end
