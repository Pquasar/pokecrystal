BattleCommand_KnockOff:
	ldh a, [hBattleTurn]
	and a
	jr nz, .enemy


	call .enemyitem
	ld a, [hl]
	and a
	ret z

; The enemy needs to have an item to steal.

	call .enemyitem
	ld a, [hl]
	and a
	ret z

	ld [wNamedObjectIndex], a

	ld a, [wEffectFailed]
	and a
	ret nz

	ld a, [wLinkMode]
	and a
	jr z, .stealenemyitem

	ld a, [wBattleMode]
	dec a
	ret z

.stealenemyitem
	call .enemyitem
	xor a
	ld [hl], a
	ld [de], a
	ld a, [wNamedObjectIndex]
	jr .stole

.enemy

; The player must have an item to steal.

	call .playeritem
	ld a, [hl]
	and a
	ret z

	ld [wNamedObjectIndex], a

	ld a, [wEffectFailed]
	and a
	ret nz

; If the enemy steals your item,
; it's gone for good if you don't get it back.

	call .playeritem
	xor a
	ld [hl], a
	ld [de], a
	ld a, [wNamedObjectIndex]

.stole
	call GetItemName
	ld hl, KnockOffText
	jp StdBattleTextbox

.playeritem
	ld a, MON_ITEM
	call BattlePartyAttr
	ld d, h
	ld e, l
	ld hl, wBattleMonItem
	ret

.enemyitem
	ld a, MON_ITEM
	call OTPartyAttr
	ld d, h
	ld e, l
	ld hl, wEnemyMonItem
	ret
