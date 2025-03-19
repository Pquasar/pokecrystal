BattleCommand_DoBurn:
	ld hl, DoesntAffectText
	ld a, [wTypeModifier]
	and EFFECTIVENESS_MASK
	jp z, .failed

	call CheckIfTargetIsFireType
	jp z, .failed

	ld a, BATTLE_VARS_STATUS_OPP
	call GetBattleVar
	ld b, a
	ld hl, AlreadyBurnedText
	and 1 << BRN
	jp nz, .failed

	call GetOpponentItem
	ld a, b
	cp HELD_PREVENT_BURN
	jr nz, .do_burn
	ld a, [hl]
	ld [wNamedObjectIndex], a
	call GetItemName
	ld hl, ProtectedByText
	jr .failed

.do_burn
	ld hl, DidntAffect1Text
	ld a, BATTLE_VARS_STATUS_OPP
	call GetBattleVar
	and a
	jr nz, .failed

	call CheckSubstituteOpp
	jr nz, .failed
	ld a, [wAttackMissed]
	and a
	jr nz, .failed


	call .apply_burn
	ld hl, WasBurnedText
	call StdBattleTextbox
	jr .finished

.finished
	farcall UseHeldStatusHealingItem
	ret

.failed
	push hl
	call AnimateFailedMove
	pop hl
	jp StdBattleTextbox

.apply_burn
	call AnimateCurrentMove
	call BurnOpponent
	jp RefreshBattleHuds

CheckIfTargetIsFireType:
	ld de, wEnemyMonType1
	ldh a, [hBattleTurn]
	and a
	jr z, .ok
	ld de, wBattleMonType1
.ok
	ld a, [de]
	inc de
	cp FIRE
	ret z
	ld a, [de]
	cp FIRE
	ret

BurnOpponent:
	ld a, BATTLE_VARS_STATUS_OPP
	call GetBattleVarAddr
	set BRN, [hl]
	ld hl, ApplyBrnEffectOnAttack
	call CallBattleCore
	jp UpdateOpponentInParty
