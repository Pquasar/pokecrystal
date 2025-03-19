; Used by GetMoveGrammar (see engine/battle/used_move_text.asm)
; Each move is given an identifier for what usedmovetext to use (0-4).
; Made redundant in English localization, where all are just "[mon]<LINE>used [move]!"

MoveGrammar:
; 0: originally "[mon]の<LINE>[move]を　つかった!" ("[mon]<LINE>used [move]!")
	db SWORDS_DANCE
	db 0 ; end set

; 1: originally "[mon]の<LINE>[move]した!" ("[mon]<LINE>did [move]!")
	db RECOVER
	db 0 ; end set

; 2: originally "[mon]の<LINE>[move]を　した!" ("[mon]<LINE>did [move]!")
	db AGILITY
	db 0 ; end set

; 3: originally "[mon]の<LINE>[move]　こうげき!" ("[mon]'s<LINE>[move] attack!")
	db POUND
	db 0 ; end set

; 4: originally "[mon]の<LINE>[move]!" ("[mon]'s<LINE>[move]!")
; Any move not listed above uses this grammar.
	db -1 ; end
