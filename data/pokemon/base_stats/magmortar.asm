	db MAGMORTAR ; 228

	db  75,  95,  67,  83, 125,  95
	;   hp  atk  def  spd  sat  sdf

	db FIRE, FIRE ; type
	db 30 ; catch rate
	db 199 ; base exp
	db NO_ITEM, NO_ITEM ; items
	db GENDER_F25 ; gender ratio
	db 100 ; unknown 1
	db 20 ; step cycles to hatch
	db 5 ; unknown 2
	INCBIN "gfx/pokemon/magmortar/front.dimensions"
	dw NULL, NULL ; unused (beta front/back pics)
	db GROWTH_SLOW ; growth rate
	dn EGG_HUMANSHAPE, EGG_HUMANSHAPE ; egg groups

	; tm/hm learnset
	tmhm TOXIC, HIDDEN_POWER, SUNNY_DAY, HYPER_BEAM, PROTECT, SOLARBEAM, THUNDERBOLT, EARTHQUAKE, RETURN, PSYCHIC_M, BRICK_BREAK, FIRE_BLAST, REST, ATTRACT, THIEF, OVERHEAT, FOCUS_BLAST, WILL_O_WISP, BULLDOZE, ROCK_SLIDE, SLEEP_TALK, SUBSTITUTE, DUAL_CHOP, FIRE_PUNCH, HEAT_WAVE, IRON_TAIL, LOW_KICK, THUNDERPUNCH
	; end
