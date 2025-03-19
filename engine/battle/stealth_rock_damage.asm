GetStealthRockDamage:

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
; pointer c, c = 1 = 1/32, c = 5 = 1/2
	ld a, [de]
	call CheckTypeList
; compare type 1 to 2
	ld b, a
	inc de
	ld a, [de]
	cp b
	jr z, .final
; skip if the same
	call CheckTypeList
.final
	ld a, c
	pop bc
	dec a
	jr z, .one
	dec a
	jr z, .two
	dec a
	jr z, .three
	dec a
	jr z, .four
	jp GetHalfMaxHP
.four
	jp GetQuarterMaxHP
.three
	jp GetEighthMaxHP
.two
	jp GetSixteenthMaxHP
.one
	jp GetThirtySecondMaxHP

CheckTypeList:
	cp FLYING
	jr z, .strong
	cp FIRE
	jr z, .strong
	cp BUG
	jr z, .strong
	cp ICE
	jr z, .strong
	cp GROUND
	jr z, .weak
	cp STEEL
	jr z, .weak
	cp FIGHTING
	ret nz
.weak
	dec c
	ret
.strong
	inc c
	ret
