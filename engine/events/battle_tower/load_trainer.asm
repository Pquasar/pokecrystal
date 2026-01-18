LoadOpponentTrainerAndPokemon:
	ldh a, [rWBK]
	push af
	ld a, BANK(wBT_OTTrainer)
	ldh [rWBK], a

; Fill wBT_OTTrainer with zeros
	xor a
	ld hl, wBT_OTTrainer
	ld bc, BATTLE_TOWER_STRUCT_LENGTH
	call ByteFill ; fill bc bytes with the value of a, starting at hl

; Write $ff into the Item-Slots
	ld a, $ff
	ld [wBT_OTMon1Item], a
	ld [wBT_OTMon2Item], a
	ld [wBT_OTMon3Item], a

; Set wBT_OTTrainer as start address to write the following data to
	ld de, wBT_OTTrainer

	ldh a, [hRandomAdd]
	ld b, a
.resample ; loop to find a random trainer
	call Random
	ldh a, [hRandomAdd]
	add b
	ld b, a ; b contains the nr of the trainer
	maskbits BATTLETOWER_NUM_UNIQUE_TRAINERS
	cp BATTLETOWER_NUM_UNIQUE_TRAINERS
	jr nc, .resample
	ld b, a ; Unnecessary?

	ld a, BANK(sBTTrainers)
	call OpenSRAM

	ld c, BATTLETOWER_STREAK_LENGTH
	ld hl, sBTTrainers
.next_trainer
	ld a, [hli]
	cp b
	jr z, .resample
	dec c
	jr nz, .next_trainer ; c <= 7  initialise all 7 trainers?

	ld hl, sBTTrainers
	ld a, [sNrOfBeatenBattleTowerTrainers]
	ld c, a
	ld a, b ; a now contains the nr of the trainer
	ld b, 0
	add hl, bc
	ld [hl], a

	call CloseSRAM

	push af
; Copy name (10 bytes) and class (1 byte) of trainer
	ld hl, BattleTowerTrainers
	ld bc, NAME_LENGTH
	call AddNTimes
	ld bc, NAME_LENGTH
	call CopyBytes

	call LoadRandomBattleTowerMon
	pop af

	ld hl, BattleTowerTrainerData
	ld bc, BATTLETOWER_TRAINERDATALENGTH
	call AddNTimes ; Add bc * a to hl
	ld bc, BATTLETOWER_TRAINERDATALENGTH
.copy_bt_trainer_data_loop
	ld a, BANK(BattleTowerTrainerData)
	call GetFarByte
	ld [de], a
	inc hl
	inc de
	dec bc
	ld a, b
	or c
	jr nz, .copy_bt_trainer_data_loop

	pop af
	ldh [rWBK], a

	ret

LoadRandomBattleTowerMon:
	ld c, BATTLETOWER_PARTY_LENGTH ; 3
.loop
	push bc
	ld a, BANK(sBTMonOfTrainers)
	call OpenSRAM

.FindARandomBattleTowerMon:
	xor a ; right now just skip 0 mons since we don't have different tiers

	ld hl, BattleTowerMons
	ld bc, BATTLETOWER_NUM_UNIQUE_MON * NICKNAMED_MON_STRUCT_LENGTH
	call AddNTimes ; Add bc * a to hl, skips over all mon below selected level group

	ldh a, [hRandomAdd]
	ld b, a
.resample
	call Random
	ldh a, [hRandomAdd]
	add b
	ld b, a ; b holds the nr of the current mon
	maskbits 145 ; current number of PU mons to test with
	cp 145
	jr nc, .resample

	ld bc, 8 ; species + item + moves + dvs bytes in the custom struct
	call AddNTimes ; Add bc * a to hl, gets the random pokemon pointer

	ld a, [hl] ; hl points to the first byte, species
	ld b, a

	ld a, [wBT_OTMon1] ; Check for any copies of the mon in the team
	cp b
	jr z, .FindARandomBattleTowerMon
	ld a, [wBT_OTMon2]
	cp b
	jr z, .FindARandomBattleTowerMon
	ld a, [wBT_OTMon3]
	cp b
	jr z, .FindARandomBattleTowerMon

; We could replace the part loading the entire struct with a template that maxes stat exp and happiness
; But for now just do it manually

	ld a, [de] ; load species in
    ld [wCurSpecies], a

; Species
	ld bc, 1
	ld a, MON_SPECIES
	call CopyBCBytes ; Copies bc bytes from hl to de+a. Preserves de but (deliberately) not hl.

; Item
	ld bc, 1
	ld a, MON_ITEM
	call CopyBCBytes

; Moves
	ld bc, NUM_MOVES
	ld a, MON_MOVES
	call CopyBCBytes

; DVs
	ld bc, 2
	ld a, MON_DVS
	call CopyBCBytes

; PP
	ld hl, MON_MOVES
	add hl, de ; Load hl the ram location for the 4 moves
	push de ; save de to be the first byte of the ram struct
	push hl
	ld hl, MON_PP
	add hl, de
	ld d, h
	ld e, l ; load de the ram location for the 4 pp locations
	pop hl
	predef FillPP
	pop de

; Stat EXP, confirmed working
	ld a, $FF
	ld hl, MON_STAT_EXP
	add hl, de
	ld bc, 10
	call ByteFill ; fill bc bytes with the value of a, starting at hl

; Happiness
	ld hl, MON_HAPPINESS
	add hl, de
	ld [hl], $FF

; Level
	ld hl, MON_LEVEL
	add hl, de
	ld [hl], 50

	ld a, 50 ; load level in
    ld [wCurPartyLevel], a

; Clear Status
	xor a
	ld hl, MON_STATUS
	add hl, de
	ld bc, 2
	call ByteFill

; Stats
;	push hl
;	ld hl, MON_STAT_EXP
;	add hl, de ; Load hl the location for the stat exp
;	push de ; save de to be the first byte of the mon struct
;	push hl
;	ld hl, MON_STATS
;	add hl, de
;	ld d, h
;	ld e, l ; load de the location for the 7 stat values
;	pop hl
;	ld b, TRUE
;	predef CalcMonStats
; Calculates all 6 Stats of a mon
; b: Take into account stat EXP if TRUE
; 'c' counts from 1-6 and points with 'wBaseStats' to the base value
; hl is the path to the Stat EXP
; de points to where the final stats will be saved
;	pop de
;	pop hl

; Store the max hp into current hp
	push de
	ld hl, MON_MAXHP
	add hl, de
	ld b, h
	ld c, l
	ld hl, MON_HP
	add hl, de
	ld a, [bc]
	inc bc
	ld [hli], a
	ld a, [bc]
	ld [hl], a
	pop de


	ld a, e
    add a, NICKNAMED_MON_STRUCT_LENGTH
    ld e, a
    jr nc, .next_mon
    inc d
.next_mon

	xor a
    ld [wCurSpecies], a
    ld [wCurPartyLevel], a

; de now points to wBT_OTMon(n+1)
; Since NICKNAMED_MON_STRUCT_LENGTH fits wBT_OTMon(n) and wBT_OTMon(n)Name

	ld a, [wNamedObjectIndex]
	push af
	push de
	ld hl, -NICKNAMED_MON_STRUCT_LENGTH ; hl points to de - NICKNAMED_MON_STRUCT_LENGTH
	add hl, de
	ld a, [hl]
	ld [wNamedObjectIndex], a
	ld bc, PARTYMON_STRUCT_LENGTH
	add hl, bc
	push hl
	call GetPokemonName ; Get Pokemon name for wNamedObjectIndex
	ld h, d
	ld l, e
	pop de
	ld bc, MON_NAME_LENGTH
	call CopyBytes ; copy MON_NAME_LENGTH bytes from hl to de

	pop de
	pop af
	ld [wNamedObjectIndex], a
	pop bc
; Register c stores BATTLETOWER_PARTY_LENGTH
; Check if 0 to know 3 pokemon were sampled
	dec c
	jp nz, .loop

	ld a, [sBTMonPrevTrainer1]
	ld [sBTMonPrevPrevTrainer1], a
	ld a, [sBTMonPrevTrainer2]
	ld [sBTMonPrevPrevTrainer2], a
	ld a, [sBTMonPrevTrainer3]
	ld [sBTMonPrevPrevTrainer3], a
	ld a, [wBT_OTMon1]
	ld [sBTMonPrevTrainer1], a
	ld a, [wBT_OTMon2]
	ld [sBTMonPrevTrainer2], a
	ld a, [wBT_OTMon3]
	ld [sBTMonPrevTrainer3], a
	call CloseSRAM
	ret

CopyBCBytes:
; Copies bc bytes from hl to de+a. Preserves de but (deliberately) not hl.
	push de
	add e
	ld e, a
	adc d
	sub e
	ld d, a
	call CopyBytes
	pop de
	ret

INCLUDE "data/battle_tower/classes.asm"

INCLUDE "data/battle_tower/parties.asm"
