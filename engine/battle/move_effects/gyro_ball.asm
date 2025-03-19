BattleCommand_GyroBall:
	push bc
	push de
	ld a, [wBattleMonSpeed + 1]
	ld c, a
	ld a, [wBattleMonSpeed]
	ld b, a
	ld a, [wEnemyMonSpeed + 1]
	ld e, a
	ld a, [wEnemyMonSpeed]
	ld d, a
	;player speed in bc, enemy speed in de if players turn
	ldh a, [hBattleTurn]
	and a
	jr z, .go
	ld a, [wEnemyMonSpeed + 1]
	ld c, a
	ld a, [wEnemyMonSpeed]
	ld b, a
	ld a, [wBattleMonSpeed + 1]
	ld e, a
	ld a, [wBattleMonSpeed]
	ld d, a
	;enemy speed in bc, player speed in de if enemy turn
.go
	ld a, b
	or c
	ld a, 1
	jr z, .got_power
rept 3
	srl b
	rr c
	srl d
	rr e
endr
	xor a
	ldh [hMultiplicand + 0], a
	ld a, d
	ldh [hMultiplicand + 1], a
	ld a, e
	ldh [hMultiplicand + 2], a
	ld a, 25
	ldh [hMultiplier], a
	call Multiply

	ld a, c
	ldh [hDivisor], a
	ld b, 4
	call Divide
	ld hl, hMultiplicand
	ld a, [hli]
	or [hl]
	ld a, 150
	jr nz, .got_power
	inc hl
	ld a, [hl]
	and a
	jr nz, .nonzero_power
	ld a, 1
	jr .got_power

.nonzero_power
	cp 151
	jr c, .got_power

	ld a, 150
.got_power
	pop de
	ld d, a
	ld [wTextDecimalByte], a
	ld hl, MagnitudeText
	call StdBattleTextbox
	pop bc
	ret
