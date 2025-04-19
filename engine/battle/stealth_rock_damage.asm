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
; Pointer c, c = 1 = 1/32, c = 5 = 1/2
	ld a, [de]
	call CheckTypeList
; Compare type 1 to 2
	ld b, a
	inc de
	ld a, [de]
	cp b
; Skip if the same
	call nz, CheckTypeList
	ld a, c
	pop bc
	dec a
	jp z, GetThirtySecondMaxHP
	dec a
	jp z, GetSixteenthMaxHP
	dec a
	jp z, GetEighthMaxHP
	dec a
	jp z, GetQuarterMaxHP
	jp GetHalfMaxHP

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
