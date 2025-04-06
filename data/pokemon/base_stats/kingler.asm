	db KINGLER ; 099

	db  55, 130, 115,  75,  50,  50
	;   hp  atk  def  spd  sat  sdf

	db WATER, WATER ; type
	db 60 ; catch rate
	db 206 ; base exp
	db NO_ITEM, NO_ITEM ; items
	db GENDER_F50 ; gender ratio
	db 100 ; unknown 1
	db 20 ; step cycles to hatch
	db 5 ; unknown 2
	INCBIN "gfx/pokemon/kingler/front.dimensions"
	dw NULL, NULL ; unused (beta front/back pics)
	db GROWTH_MEDIUM_FAST ; growth rate
	dn EGG_WATER_3, EGG_WATER_3 ; egg groups

	; tm/hm learnset
	tmhm TOXIC, HAIL, HIDDEN_POWER, ICE_BEAM, BLIZZARD, HYPER_BEAM, PROTECT, RAIN_DANCE, RETURN, BRICK_BREAK, REST, ATTRACT, THIEF, SCALD, SWORDS_DANCE, ROCK_SLIDE, X_SCISSOR, SLEEP_TALK, SUBSTITUTE, SURF, ICY_WIND, IRON_DEFENSE, KNOCK_OFF, LIQUIDATION
	; end
