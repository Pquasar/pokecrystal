GetStealthRockDamage:

; this code should NEVER be run under any circumstance
	ld de, wBattleMonType
	ld bc, UpdatePlayerHUD
	ldh a, [hBattleTurn]
	and a
	jr z, .ok
	ld de, wEnemyMonType
	ld bc, UpdateEnemyHUD
.ok
	push bc
	ld c, 3
	ld a, [de]
	cp FLYING
	jr z, .strong1
	cp BUG
	jr z, .strong1
	cp ICE
	jr z, .strong1
	cp FIRE
	jr z, .strong1
	cp GROUND
	jr z, .weak1
	cp STEEL
	jr z, .weak1
	cp FIGHTING
	jr z, .weak1
	jr .check2

.strong1
	inc c
	jr .check2

.weak1
	dec c

.check2
	push bc
	ld a, [de]
	ld b, a
	inc de
	ld a, [de]
	cp b
	pop bc
	jr z, .check
	ld a, [de]
	cp FLYING
	jr z, .strong2
	cp BUG
	jr z, .strong2
	cp ICE
	jr z, .strong2
	cp FIRE
	jr z, .strong2
	cp GROUND
	jr z, .weak2
	cp STEEL
	jr z, .weak2
	cp FIGHTING
	jr z, .weak2
	jr .check

.weak2
	dec c
	jr .check

.strong2
	inc c

.check
	ld a, c
	pop bc
	dec a
	jr z, .one ; 1/4 effective
	dec a
	jr z, .two ; 1/2 effective
	dec a
	jr z, .three ; neutral
	dec a
	jr z, .four ; 2x effective
	jp GetHalfMaxHP
.four
	jp GetQuarterMaxHP
.three
	jp GetEighthMaxHP
.two
	jp GetSixteenthMaxHP
.one
	jp GetThirtySecondMaxHP
