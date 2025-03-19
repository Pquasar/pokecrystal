BattleCommand_Recoil:
	ld a, BATTLE_VARS_MOVE_ANIM
	call GetBattleVar
	ld d, a
	cp STRUGGLE
	jp z, .quarterhealth
	cp WILD_CHARGE
	jp z, .wildcharge
	ld a, 3
	jp .go
.wildcharge
	ld a, 4
.go
	push af
	ld hl, wBattleMonMaxHP
	ldh a, [hBattleTurn]
	and a
	jr z, .got_hp
	ld hl, wEnemyMonMaxHP
.got_hp
	ld a, BATTLE_VARS_MOVE_ANIM
	call GetBattleVar
	ld d, a
	pop af
; divide damage by parameter
	ldh [hDivisor], a
	xor a
	ldh [hDividend], a
	ldh [hDividend + 1], a
	ld a, [wCurDamage]
	ldh [hDividend + 2], a
	ld a, [wCurDamage + 1]
	ldh [hDividend + 3], a
	ld b, 4
	call Divide
	ldh a, [hQuotient + 2]
	ld b, a
	ldh a, [hQuotient + 3]
	ld c, a
; if recoil damage is 0, set to 1
	ld a, b
	or c
	jr nz, .min_damage
	inc c
.min_damage
	ld a, [hli]
	ld [wHPBuffer1 + 1], a
	ld a, [hl]
	ld [wHPBuffer1], a
	dec hl
	dec hl
	ld a, [hl]
	ld [wHPBuffer2], a
	sub c
	ld [hld], a
	ld [wHPBuffer3], a
	ld a, [hl]
	ld [wHPBuffer2 + 1], a
	sbc b
	ld [hl], a
	ld [wHPBuffer3 + 1], a
	jr nc, .dont_ko
	xor a
	ld [hli], a
	ld [hl], a
	ld hl, wHPBuffer3
	ld [hli], a
	ld [hl], a
.dont_ko
	hlcoord 10, 9
	ldh a, [hBattleTurn]
	and a
	ld a, 1
	jr z, .animate_hp_bar
	hlcoord 2, 2
	xor a
.animate_hp_bar
	ld [wWhichHPBar], a
	predef AnimateHPBar
	call RefreshBattleHuds
	ld hl, RecoilText
	jp StdBattleTextbox
.quarterhealth
	callfar GetQuarterMaxHP
	callfar SubtractHPFromUser
	ld hl, RecoilText
	jp StdBattleTextbox
