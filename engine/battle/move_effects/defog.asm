BattleCommand_Defog:
	xor a
	ld [wEnemyScreens], a
	ld [wPlayerScreens], a
	ret
	
	
