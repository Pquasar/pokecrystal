SECTION "Evolutions and Attacks", ROMX

; Evos+attacks data structure:
; - Evolution methods:
;    * db EVOLVE_LEVEL, level, species
;    * db EVOLVE_ITEM, used item, species
;    * db EVOLVE_TRADE, held item (or -1 for none), species
;    * db EVOLVE_HAPPINESS, TR_* constant (ANYTIME, MORNDAY, NITE), species
;    * db EVOLVE_STAT, level, ATK_*_DEF constant (LT, GT, EQ), species
; - db 0 ; no more evolutions
; - Learnset (in increasing level order):
;    * db level, move
; - db 0 ; no more level-up moves

INCLUDE "data/pokemon/evos_attacks_pointers.asm"

BulbasaurEvosAttacks:
	db EVOLVE_LEVEL, 16, LEAFEON
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

LeafeonEvosAttacks:
	db 0 ; no more evolutions
	db 1, SUNNY_DAY
	db 1, MOONLIGHT
	db 0 ; no more level-up moves

VenusaurEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SWORDS_DANCE
	db 1, LEECH_SEED
	db 1, POWER_WHIP
	db 4, SWORDS_DANCE
	db 7, LEECH_SEED
	db 10, POWER_WHIP
	db 15, POISONPOWDER
	db 15, SLEEP_POWDER
	db 22, GIGA_DRAIN
	db 29, SWEET_SCENT
	db 41, GROWTH
	db 53, SYNTHESIS
	db 65, SOLARBEAM
	db 0 ; no more level-up moves

CharmanderEvosAttacks:
	db EVOLVE_LEVEL, 16, SYLVEON
	db 0 ; no more evolutions
	db 1, SCRATCH
	db 0 ; no more level-up moves

SylveonEvosAttacks:
	db 0 ; no more evolutions
	db 99, CHARM
	db 99, DETECT
	db 99, QUICK_ATTACK
	db 100, MOONBLAST
	db 100, CALM_MIND
	db 100, LIGHT_SCREEN
	db 100, SHADOW_BALL
	db 0 ; no more level-up moves

CharizardEvosAttacks:
	db 0 ; no more evolutions
	db 1, SCRATCH
	db 1, SWORDS_DANCE
	db 1, FLAMETHROWER
	db 1, TACKLE
	db 7, FLAMETHROWER
	db 13, TACKLE
	db 20, RAGE
	db 27, SCARY_FACE
	db 34, FLAMETHROWER
	db 36, WING_ATTACK
	db 44, SLASH
	db 54, OVERHEAT
	db 64, FIRE_SPIN
	db 0 ; no more level-up moves

SquirtleEvosAttacks:
	db EVOLVE_LEVEL, 16, GLACEON
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

GlaceonEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SWORDS_DANCE
	db 1, TACKLE
	db 4, SWORDS_DANCE
	db 7, TACKLE
	db 10, TACKLE
	db 13, HYDRO_PUMP
	db 19, CRUNCH
	db 25, RAPID_SPIN
	db 31, PROTECT
	db 37, RAIN_DANCE
	db 45, SKULL_BASH
	db 53, HYDRO_PUMP
	db 0 ; no more level-up moves

BlastoiseEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SWORDS_DANCE
	db 1, TACKLE
	db 1, TACKLE
	db 4, SWORDS_DANCE
	db 7, TACKLE
	db 10, TACKLE
	db 13, HYDRO_PUMP
	db 19, CRUNCH
	db 25, RAPID_SPIN
	db 31, PROTECT
	db 42, RAIN_DANCE
	db 55, SKULL_BASH
	db 68, HYDRO_PUMP
	db 0 ; no more level-up moves

CaterpieEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

YanmegaEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

ButterfreeEvosAttacks:
	db 0 ; no more evolutions
	db 99, IRON_DEFENSE
	db 99, POISONPOWDER
	db 99, STUN_SPORE
	db 99, PSYBEAM
	db 99, WHIRLWIND
	db 100, QUIVER_DANCE
	db 100, BUG_BUZZ
	db 100, AIR_SLASH
	db 100, SLEEP_POWDER
	db 0 ; no more level-up moves

WeedleEvosAttacks:
	db EVOLVE_LEVEL, 7, MAMOSWINE
	db 0 ; no more evolutions
	db 1, POISON_JAB
	db 0 ; no more level-up moves

MamoswineEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

BeedrillEvosAttacks:
	db 0 ; no more evolutions
	db 99, IRON_DEFENSE
	db 99, PURSUIT
	db 99, AGILITY
	db 100, POISON_JAB
	db 100, X_SCISSOR
	db 100, ACROBATICS
	db 100, U_TURN
	db 0 ; no more level-up moves

PidgeyEvosAttacks:
	db EVOLVE_LEVEL, 18, PIDGEOTTO
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

PidgeottoEvosAttacks:
	db EVOLVE_LEVEL, 36, PIDGEOT
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

PidgeotEvosAttacks:
	db 0 ; no more evolutions
	db 99, AIR_SLASH
	db 99, PURSUIT
	db 99, STEEL_WING
	db 99, QUICK_ATTACK
	db 99, HURRICANE
	db 99, WHIRLWIND
	db 99, AGILITY
	db 100, BRAVE_BIRD
	db 100, ROOST
	db 100, RETURN
	db 100, U_TURN
	db 0 ; no more level-up moves

RattataEvosAttacks:
	db EVOLVE_LEVEL, 20, RATICATE
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

RaticateEvosAttacks:
	db 0 ; no more evolutions
	db 99, SCREECH
	db 99, SCARY_FACE
	db 99, QUICK_ATTACK
	db 99, FOCUS_ENERGY
	db 99, PURSUIT
	db 99, CRUNCH
	db 99, DOUBLE_EDGE
	db 100, RETURN
	db 100, SUCKER_PUNCH
	db 100, SWORDS_DANCE
	db 100, ZEN_HEADBUTT
	db 0 ; no more level-up moves

SpearowEvosAttacks:
	db EVOLVE_LEVEL, 20, FEAROW
	db 0 ; no more evolutions
	db 1, DRILL_PECK
	db 0 ; no more level-up moves

FearowEvosAttacks:
	db 0 ; no more evolutions
	db 99, FAINT_ATTACK
	db 99, QUICK_ATTACK
	db 99, SCARY_FACE
	db 99, STEEL_WING
	db 99, WHIRLWIND
	db 99, PURSUIT
	db 99, AGILITY
	db 99, FOCUS_ENERGY
	db 99, ROOST
	db 100, DRILL_PECK
	db 100, DRILL_RUN
	db 100, U_TURN
	db 100, RETURN
	db 0 ; no more level-up moves

EkansEvosAttacks:
	db EVOLVE_LEVEL, 22, ARBOK
	db 0 ; no more evolutions
	db 1, WRAP
	db 0 ; no more level-up moves

ArbokEvosAttacks:
	db 0 ; no more evolutions
	db 99, IRON_TAIL
	db 99, PURSUIT
	db 99, SCARY_FACE
	db 99, CRUNCH
	db 99, WRAP
	db 99, GLARE
	db 99, STOCKPILE
	db 99, ACID_SPRAY
	db 99, HAZE
	db 100, COIL
	db 100, GUNK_SHOT
	db 100, SUCKER_PUNCH
	db 100, DRAGON_TAIL
	db 0 ; no more level-up moves

PikachuEvosAttacks:
	db EVOLVE_ITEM, THUNDERSTONE, RAICHU
	db 0 ; no more evolutions
	db 1, THUNDERBOLT
	db 0 ; no more level-up moves

RaichuEvosAttacks:
	db 0 ; no more evolutions
	db 1, THUNDERBOLT
	db 1, SWORDS_DANCE
	db 1, QUICK_ATTACK
	db 1, THUNDERBOLT
	db 0 ; no more level-up moves

SandshrewEvosAttacks:
	db EVOLVE_LEVEL, 22, SANDSLASH
	db 0 ; no more evolutions
	db 1, SCRATCH
	db 0 ; no more level-up moves

SandslashEvosAttacks:
	db 0 ; no more evolutions
	db 1, SCRATCH
	db 1, DEFENSE_CURL
	db 1, GLARE
	db 6, DEFENSE_CURL
	db 11, GLARE
	db 17, POISON_JAB
	db 24, SLASH
	db 33, SWIFT
	db 42, TACKLE
	db 52, SANDSTORM
	db 0 ; no more level-up moves

NidoranFEvosAttacks:
	db EVOLVE_LEVEL, 16, NIDORINA
	db 0 ; no more evolutions
	db 1, SWORDS_DANCE
	db 0 ; no more level-up moves

NidorinaEvosAttacks:
	db EVOLVE_ITEM, MOON_STONE, NIDOQUEEN
	db 0 ; no more evolutions
	db 1, SWORDS_DANCE
	db 0 ; no more level-up moves

NidoqueenEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SCRATCH
	db 1, DOUBLE_KICK
	db 1, SWORDS_DANCE
	db 23, BODY_SLAM
	db 0 ; no more level-up moves

NidoranMEvosAttacks:
	db EVOLVE_LEVEL, 16, NIDORINO
	db 0 ; no more evolutions
	db 1, CRUNCH
	db 0 ; no more level-up moves

NidorinoEvosAttacks:
	db EVOLVE_ITEM, MOON_STONE, NIDOKING
	db 0 ; no more evolutions
	db 1, CRUNCH
	db 0 ; no more level-up moves

NidokingEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SWORDS_DANCE
	db 1, DOUBLE_KICK
	db 1, POISON_JAB
	db 23, THRASH
	db 0 ; no more level-up moves

ClefairyEvosAttacks:
	db EVOLVE_ITEM, MOON_STONE, CLEFABLE
	db 0 ; no more evolutions
	db 1, POUND
	db 0 ; no more level-up moves

ClefableEvosAttacks:
	db 0 ; no more evolutions
	db 1, HYPNOSIS
	db 1, DOUBLESLAP
	db 1, METRONOME
	db 1, MOONLIGHT
	db 0 ; no more level-up moves

VulpixEvosAttacks:
	db EVOLVE_ITEM, FIRE_STONE, NINETALES
	db 0 ; no more evolutions
	db 1, FLAMETHROWER
	db 0 ; no more level-up moves

NinetalesEvosAttacks:
	db 0 ; no more evolutions
	db 99, HEX
	db 99, HYPNOSIS
	db 99, QUICK_ATTACK
	db 100, FLAMETHROWER
	db 100, CALM_MIND
	db 100, DARK_PULSE
	db 100, ENERGY_BALL
	db 0 ; no more level-up moves

JigglypuffEvosAttacks:
	db EVOLVE_ITEM, MOON_STONE, WIGGLYTUFF
	db 0 ; no more evolutions
	db 1, HYPNOSIS
	db 0 ; no more level-up moves

WigglytuffEvosAttacks:
	db 0 ; no more evolutions
	db 99, PERISH_SONG
	db 99, DISABLE
	db 100, DAZLINGGLEAM
	db 100, TOXIC
	db 100, THUNDER_WAVE
	db 100, HEAL_BELL
	db 0 ; no more level-up moves

ZubatEvosAttacks:
	db EVOLVE_LEVEL, 22, GOLBAT
	db 0 ; no more evolutions
	db 1, LEECH_LIFE
	db 0 ; no more level-up moves

GolbatEvosAttacks:
	db EVOLVE_HAPPINESS, TR_ANYTIME, CROBAT
	db 0 ; no more evolutions
	db 1, SCREECH
	db 0 ; no more level-up moves

OddishEvosAttacks:
	db EVOLVE_LEVEL, 21, GLOOM
	db 0 ; no more evolutions
	db 1, ENERGY_BALL
	db 0 ; no more level-up moves

GloomEvosAttacks:
	db EVOLVE_ITEM, LEAF_STONE, VILEPLUME
	db EVOLVE_ITEM, SUN_STONE, BELLOSSOM
	db 0 ; no more evolutions
	db 1, ENERGY_BALL
	db 0 ; no more level-up moves

VileplumeEvosAttacks:
	db 0 ; no more evolutions
	db 1, ENERGY_BALL
	db 1, SWEET_SCENT
	db 1, STUN_SPORE
	db 1, PETAL_DANCE
	db 0 ; no more level-up moves

ParasEvosAttacks:
	db EVOLVE_LEVEL, 24, PARASECT
	db 0 ; no more evolutions
	db 1, SCRATCH
	db 0 ; no more level-up moves

ParasectEvosAttacks:
	db 0 ; no more evolutions
	db 99, AGILITY
	db 99, CROSS_POISON
	db 99, PURSUIT
	db 99, STUN_SPORE
	db 99, POISONPOWDER
	db 99, FURY_CUTTER
	db 99, SLASH
	db 99, GROWTH
	db 100, LEECH_LIFE
	db 100, SEED_BOMB
	db 100, SPORE
	db 100, SWORDS_DANCE
	db 0 ; no more level-up moves

LickilickyEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, DISABLE
	db 1, TACKLE
	db 9, SLUDGE_BOMB
	db 17, PSYWAVE
	db 20, POISONPOWDER
	db 25, LEECH_LIFE
	db 28, STUN_SPORE
	db 33, PSYBEAM
	db 36, SLEEP_POWDER
	db 41, PSYCHIC_M
	db 0 ; no more level-up moves

VenomothEvosAttacks:
	db 0 ; no more evolutions
	db 99, AGILITY
	db 99, MORNING_SUN
	db 99, PSYBEAM
	db 99, SLEEP_POWDER
	db 99, PSYCHIC_M
	db 100, GIGA_DRAIN
	db 100, BUG_BUZZ
	db 100, QUIVER_DANCE
	db 100, STUN_SPORE
	db 0 ; no more level-up moves

DiglettEvosAttacks:
	db EVOLVE_LEVEL, 26, DUGTRIO
	db 0 ; no more evolutions
	db 1, SCRATCH
	db 0 ; no more level-up moves

DugtrioEvosAttacks:
	db 0 ; no more evolutions
	db 99, SCREECH
	db 99, BULLDOZE
	db 99, SUCKER_PUNCH
	db 99, SLASH
	db 100, EARTHQUAKE
	db 100, RETURN
	db 100, NIGHT_SLASH
	db 100, PURSUIT
	db 0 ; no more level-up moves

MeowthEvosAttacks:
	db EVOLVE_LEVEL, 28, PERSIAN
	db 0 ; no more evolutions
	db 1, SCRATCH
	db 0 ; no more level-up moves

PersianEvosAttacks:
	db 0 ; no more evolutions
	db 99, AMNESIA
	db 99, CHARM
	db 99, FLAIL
	db 99, HYPNOSIS
	db 99, IRON_TAIL
	db 99, FAKE_OUT
	db 100, PLAY_ROUGH
	db 100, NIGHT_SLASH
	db 100, RETURN
	db 100, U_TURN
	db 0 ; no more level-up moves

PsyduckEvosAttacks:
	db EVOLVE_LEVEL, 33, GOLDUCK
	db 0 ; no more evolutions
	db 1, SCRATCH
	db 0 ; no more level-up moves

GolduckEvosAttacks:
	db 0 ; no more evolutions
	db 1, SCRATCH
	db 1, SWORDS_DANCE
	db 1, DISABLE
	db 1, PSYWAVE
	db 5, SWORDS_DANCE
	db 10, DISABLE
	db 16, PSYWAVE
	db 23, SCREECH
	db 31, PSYWAVE
	db 44, TACKLE
	db 58, HYDRO_PUMP
	db 0 ; no more level-up moves

MankeyEvosAttacks:
	db EVOLVE_LEVEL, 28, PRIMEAPE
	db 0 ; no more evolutions
	db 1, SCRATCH
	db 0 ; no more level-up moves

PrimeapeEvosAttacks:
	db 0 ; no more evolutions
	db 99, ENCORE
	db 99, NIGHT_SLASH
	db 99, LOW_KICK
	db 99, PURSUIT
	db 99, SEISMIC_TOSS
	db 99, CROSS_CHOP
	db 100, CLOSE_COMBAT
	db 100, OUTRAGE
	db 100, BULK_UP
	db 100, ROCK_SLIDE
	db 0 ; no more level-up moves

GrowlitheEvosAttacks:
	db EVOLVE_ITEM, FIRE_STONE, ARCANINE
	db 0 ; no more evolutions
	db 1, CRUNCH
	db 0 ; no more level-up moves

ArcanineEvosAttacks:
	db 0 ; no more evolutions
	db 1, ROAR
	db 1, CRUNCH
	db 1, SWORDS_DANCE
	db 1, TACKLE
	db 50, EXTREMESPEED
	db 0 ; no more level-up moves

PoliwagEvosAttacks:
	db EVOLVE_LEVEL, 25, POLIWHIRL
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

PoliwhirlEvosAttacks:
	db EVOLVE_ITEM, WATER_STONE, POLIWRATH
	db EVOLVE_TRADE, KINGS_ROCK, POLITOED
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

PoliwrathEvosAttacks:
	db 0 ; no more evolutions
	db 1, HYDRO_PUMP
	db 1, HYPNOSIS
	db 1, DOUBLESLAP
	db 1, GIGA_DRAIN
	db 35, GIGA_DRAIN
	db 51, TACKLE
	db 0 ; no more level-up moves

AbraEvosAttacks:
	db EVOLVE_LEVEL, 16, KADABRA
	db 0 ; no more evolutions
	db 1, TELEPORT
	db 0 ; no more level-up moves

KadabraEvosAttacks:
	db EVOLVE_TRADE, -1, ALAKAZAM
	db 0 ; no more evolutions
	db 1, TELEPORT
	db 0 ; no more level-up moves

AlakazamEvosAttacks:
	db 0 ; no more evolutions
	db 1, TELEPORT
	db 1, TACKLE
	db 1, PSYWAVE
	db 16, PSYWAVE
	db 18, DISABLE
	db 21, PSYBEAM
	db 26, RECOVER
	db 31, FUTURE_SIGHT
	db 38, PSYCHIC_M
	db 45, REFLECT
	db 0 ; no more level-up moves

MachopEvosAttacks:
	db EVOLVE_LEVEL, 28, MACHOKE
	db 0 ; no more evolutions
	db 1, LOW_KICK
	db 0 ; no more level-up moves

MachokeEvosAttacks:
	db EVOLVE_TRADE, -1, MACHAMP
	db 0 ; no more evolutions
	db 1, LOW_KICK
	db 0 ; no more level-up moves

MachampEvosAttacks:
	db 0 ; no more evolutions
	db 1, LOW_KICK
	db 1, CRUNCH
	db 1, FOCUS_ENERGY
	db 8, FOCUS_ENERGY
	db 15, KARATE_CHOP
	db 19, SEISMIC_TOSS
	db 25, TACKLE
	db 34, CROSS_CHOP
	db 43, CROSS_CHOP
	db 52, SCARY_FACE
	db 61, GIGA_DRAIN
	db 0 ; no more level-up moves

HonchkrowEvosAttacks:
	db 0 ; no more evolutions
	db 1, POWER_WHIP
	db 6, GROWTH
	db 11, WRAP
	db 15, SLEEP_POWDER
	db 17, POISONPOWDER
	db 19, STUN_SPORE
	db 23, SLUDGE_BOMB
	db 30, SWEET_SCENT
	db 37, GIGA_DRAIN
	db 45, BODY_SLAM
	db 0 ; no more level-up moves

WeepinbellEvosAttacks:
	db EVOLVE_ITEM, LEAF_STONE, VICTREEBEL
	db 0 ; no more evolutions
	db 1, POWER_WHIP
	db 1, GROWTH
	db 1, WRAP
	db 6, GROWTH
	db 11, WRAP
	db 15, SLEEP_POWDER
	db 17, POISONPOWDER
	db 19, STUN_SPORE
	db 24, SLUDGE_BOMB
	db 33, SWEET_SCENT
	db 42, GIGA_DRAIN
	db 54, BODY_SLAM
	db 0 ; no more level-up moves

VictreebelEvosAttacks:
	db 0 ; no more evolutions
	db 1, POWER_WHIP
	db 1, SLEEP_POWDER
	db 1, SWEET_SCENT
	db 1, GIGA_DRAIN
	db 0 ; no more level-up moves

TentacoolEvosAttacks:
	db EVOLVE_LEVEL, 30, TENTACRUEL
	db 0 ; no more evolutions
	db 1, POISON_JAB
	db 6, SLUDGE_BOMB
	db 12, TACKLE
	db 19, SLUDGE_BOMB
	db 25, BUBBLEBEAM
	db 30, WRAP
	db 36, BARRIER
	db 43, SCREECH
	db 49, HYDRO_PUMP
	db 0 ; no more level-up moves

TentacruelEvosAttacks:
	db 0 ; no more evolutions
	db 1, POISON_JAB
	db 1, SLUDGE_BOMB
	db 1, TACKLE
	db 6, SLUDGE_BOMB
	db 12, TACKLE
	db 19, SLUDGE_BOMB
	db 25, BUBBLEBEAM
	db 30, WRAP
	db 38, BARRIER
	db 47, SCREECH
	db 55, HYDRO_PUMP
	db 0 ; no more level-up moves

GeodudeEvosAttacks:
	db EVOLVE_LEVEL, 25, GRAVELER
	db 0 ; no more evolutions
	db 1, TACKLE
	db 6, DEFENSE_CURL
	db 11, ROCK_SLIDE
	db 16, MAGNITUDE
	db 21, SELFDESTRUCT
	db 26, TACKLE
	db 31, ROLLOUT
	db 36, EARTHQUAKE
	db 41, EXPLOSION
	db 0 ; no more level-up moves

GravelerEvosAttacks:
	db EVOLVE_TRADE, -1, GOLEM
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, DEFENSE_CURL
	db 1, ROCK_SLIDE
	db 6, DEFENSE_CURL
	db 11, ROCK_SLIDE
	db 16, MAGNITUDE
	db 21, SELFDESTRUCT
	db 27, TACKLE
	db 34, ROLLOUT
	db 41, EARTHQUAKE
	db 48, EXPLOSION
	db 0 ; no more level-up moves

GolemEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, DEFENSE_CURL
	db 1, ROCK_SLIDE
	db 1, MAGNITUDE
	db 6, DEFENSE_CURL
	db 11, ROCK_SLIDE
	db 16, MAGNITUDE
	db 21, SELFDESTRUCT
	db 27, TACKLE
	db 34, ROLLOUT
	db 41, EARTHQUAKE
	db 48, EXPLOSION
	db 0 ; no more level-up moves

PonytaEvosAttacks:
	db EVOLVE_LEVEL, 40, RAPIDASH
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

RapidashEvosAttacks:
	db 0 ; no more evolutions
	db 99, DOUBLE_KICK
	db 99, DOUBLE_EDGE
	db 99, HYPNOSIS
	db 99, POISON_JAB
	db 99, MEGAHORN
	db 99, QUICK_ATTACK
	db 99, AGILITY
	db 100, FLARE_BLITZ
	db 100, WILD_CHARGE
	db 100, LOW_KICK
	db 100, MORNING_SUN
	db 0 ; no more level-up moves

SlowpokeEvosAttacks:
	db EVOLVE_LEVEL, 37, SLOWBRO
	db EVOLVE_TRADE, KINGS_ROCK, SLOWKING
	db 0 ; no more evolutions
	db 1, CURSE
	db 1, TACKLE
	db 6, SWORDS_DANCE
	db 15, HYDRO_PUMP
	db 20, PSYWAVE
	db 29, DISABLE
	db 34, DRAGON_CLAW
	db 43, AMNESIA
	db 48, PSYCHIC_M
	db 0 ; no more level-up moves

SlowbroEvosAttacks:
	db 0 ; no more evolutions
	db 1, CURSE
	db 1, TACKLE
	db 1, SWORDS_DANCE
	db 1, HYDRO_PUMP
	db 6, SWORDS_DANCE
	db 15, HYDRO_PUMP
	db 20, PSYWAVE
	db 29, DISABLE
	db 34, DRAGON_CLAW
	db 37, TACKLE
	db 46, AMNESIA
	db 54, PSYCHIC_M
	db 0 ; no more level-up moves

MagnemiteEvosAttacks:
	db EVOLVE_LEVEL, 30, MAGNETON
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

MagnetonEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

FarfetchDEvosAttacks:
	db 0 ; no more evolutions
	db 99, CURSE
	db 99, FLAIL
	db 99, QUICK_ATTACK
	db 99, ROOST
	db 99, STEEL_WING
	db 99, POISON_JAB
	db 99, FURY_CUTTER
	db 99, SLASH
	db 99, AGILITY
	db 100, BRAVE_BIRD
	db 100, SWORDS_DANCE
	db 100, LEAF_BLADE
	db 100, NIGHT_SLASH
	db 0 ; no more level-up moves

DoduoEvosAttacks:
	db EVOLVE_LEVEL, 31, DODRIO
	db 0 ; no more evolutions
	db 1, DRILL_PECK
	db 0 ; no more level-up moves

DodrioEvosAttacks:
	db 0 ; no more evolutions
	db 1, DRILL_PECK
	db 1, SWORDS_DANCE
	db 1, PURSUIT
	db 1, SWORDS_DANCE
	db 9, PURSUIT
	db 13, SWORDS_DANCE
	db 21, TRI_ATTACK
	db 25, RAGE
	db 38, DRILL_PECK
	db 47, AGILITY
	db 0 ; no more level-up moves

SeelEvosAttacks:
	db EVOLVE_LEVEL, 34, DEWGONG
	db 0 ; no more evolutions
	db 1, DRAGON_CLAW
	db 0 ; no more level-up moves

DewgongEvosAttacks:
	db 0 ; no more evolutions
	db 99, ENCORE
	db 99, IRON_TAIL
	db 99, PERISH_SONG
	db 99, STOCKPILE
	db 99, SLEEP_TALK
	db 99, ICE_SHARD
	db 99, AQUA_JET
	db 99, AQUA_TAIL
	db 100, SURF
	db 100, ICE_BEAM
	db 100, REST
	db 100, SIGNAL_BEAM
	db 0 ; no more level-up moves

GrimerEvosAttacks:
	db EVOLVE_LEVEL, 38, MUK
	db 0 ; no more evolutions
	db 1, POISON_GAS
	db 1, POUND
	db 5, TACKLE
	db 10, DISABLE
	db 16, SLUDGE
	db 23, MINIMIZE
	db 31, SCREECH
	db 40, ACID_ARMOR
	db 50, SLUDGE_BOMB
	db 0 ; no more level-up moves

MukEvosAttacks:
	db 0 ; no more evolutions
	; moves are not sorted by level
	db 1, POISON_GAS
	db 1, POUND
	db 1, TACKLE
	db 33, TACKLE
	db 37, DISABLE
	db 45, SLUDGE
	db 23, MINIMIZE
	db 31, SCREECH
	db 45, ACID_ARMOR
	db 60, SLUDGE_BOMB
	db 0 ; no more level-up moves

ShellderEvosAttacks:
	db EVOLVE_ITEM, WATER_STONE, CLOYSTER
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, TACKLE
	db 9, SLUDGE_BOMB
	db 17, AURORA_BEAM
	db 25, PROTECT
	db 33, CRUNCH
	db 41, CLAMP
	db 49, ICE_BEAM
	db 0 ; no more level-up moves

CloysterEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SLUDGE_BOMB
	db 1, AURORA_BEAM
	db 1, PROTECT
	db 33, SPIKES
	db 41, TACKLE
	db 0 ; no more level-up moves

GastlyEvosAttacks:
	db EVOLVE_LEVEL, 25, HAUNTER
	db 0 ; no more evolutions
	db 1, HYPNOSIS
	db 1, TACKLE
	db 8, CURSE
	db 13, MEAN_LOOK
	db 16, CURSE
	db 21, NIGHT_SHADE
	db 28, CONFUSE_RAY
	db 33, DREAM_EATER
	db 36, DESTINY_BOND
	db 0 ; no more level-up moves

HaunterEvosAttacks:
	db EVOLVE_TRADE, -1, GENGAR
	db 0 ; no more evolutions
	db 1, HYPNOSIS
	db 0 ; no more level-up moves

GengarEvosAttacks:
	db 0 ; no more evolutions
	db 99, HAZE
	db 99, HYPNOSIS
	db 99, CURSE
	db 99, NIGHT_SHADE
	db 99, SUCKER_PUNCH
	db 99, DESTINY_BOND
	db 99, HEX
	db 100, DARK_PULSE
	db 100, SHADOW_BALL
	db 100, SLUDGE_BOMB
	db 100, WILL_O_WISP
	db 0 ; no more level-up moves

OnixEvosAttacks:
	db EVOLVE_TRADE, METAL_COAT, STEELIX
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

GliscorEvosAttacks:
	db 0 ; no more evolutions
	db 99, AGILITY
	db 99, DOUBLE_EDGE
	db 99, NIGHT_SLASH
	db 99, POISON_JAB
	db 99, X_SCISSOR
	db 99, SWORDS_DANCE
	db 100, U_TURN
	db 100, ACROBATICS
	db 100, ROOST
	db 100, EARTHQUAKE
	db 0 ; no more level-up moves

HypnoEvosAttacks:
	db 0 ; no more evolutions
	db 1, POUND
	db 1, HYPNOSIS
	db 1, DISABLE
	db 1, PSYWAVE
	db 10, DISABLE
	db 18, PSYWAVE
	db 25, DRAGON_CLAW
	db 33, POISON_GAS
	db 40, TACKLE
	db 49, PSYCHIC_M
	db 55, PSYWAVE
	db 60, FUTURE_SIGHT
	db 0 ; no more level-up moves

KrabbyEvosAttacks:
	db EVOLVE_LEVEL, 28, KINGLER
	db 0 ; no more evolutions
	db 100, CRABHAMMER
	db 0 ; no more level-up moves

KinglerEvosAttacks:
	db 0 ; no more evolutions
	db 99, AGILITY
	db 99, AMNESIA
	db 99, FLAIL
	db 99, PROTECT
	db 100, CRABHAMMER
	db 100, X_SCISSOR
	db 100, SWORDS_DANCE
	db 100, BRICK_BREAK
	db 0 ; no more level-up moves

VoltorbEvosAttacks:
	db EVOLVE_LEVEL, 30, ELECTRODE
	db 0 ; no more evolutions
	db 1, TACKLE
	db 9, SCREECH
	db 17, SLUDGE_BOMB
	db 23, SELFDESTRUCT
	db 29, ROLLOUT
	db 33, LIGHT_SCREEN
	db 37, SWIFT
	db 39, EXPLOSION
	db 41, MIRROR_COAT
	db 0 ; no more level-up moves

ElectrodeEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SCREECH
	db 1, SLUDGE_BOMB
	db 1, SELFDESTRUCT
	db 9, SCREECH
	db 17, SLUDGE_BOMB
	db 23, SELFDESTRUCT
	db 29, ROLLOUT
	db 34, LIGHT_SCREEN
	db 40, SWIFT
	db 44, EXPLOSION
	db 48, MIRROR_COAT
	db 0 ; no more level-up moves

ExeggcuteEvosAttacks:
	db EVOLVE_ITEM, LEAF_STONE, EXEGGUTOR
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, HYPNOSIS
	db 7, REFLECT
	db 13, LEECH_SEED
	db 19, PSYWAVE
	db 25, STUN_SPORE
	db 31, POISONPOWDER
	db 37, SLEEP_POWDER
	db 43, SOLARBEAM
	db 0 ; no more level-up moves

ExeggutorEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, HYPNOSIS
	db 1, PSYWAVE
	db 19, ZEN_HEADBUTT
	db 31, TACKLE
	db 0 ; no more level-up moves

CuboneEvosAttacks:
	db EVOLVE_LEVEL, 28, MAROWAK
	db 0 ; no more evolutions
	db 1, SWORDS_DANCE
	db 5, SWORDS_DANCE
	db 9, TACKLE
	db 13, DRAGON_CLAW
	db 17, CRUNCH
	db 21, FOCUS_ENERGY
	db 25, BONEMERANG
	db 29, RAGE
	db 33, TACKLE
	db 37, THRASH
	db 41, EARTHQUAKE
	db 0 ; no more level-up moves

MarowakEvosAttacks:
	db 0 ; no more evolutions
	db 1, SWORDS_DANCE
	db 1, SWORDS_DANCE
	db 1, TACKLE
	db 1, DRAGON_CLAW
	db 5, SWORDS_DANCE
	db 9, TACKLE
	db 13, DRAGON_CLAW
	db 17, CRUNCH
	db 21, FOCUS_ENERGY
	db 25, BONEMERANG
	db 32, RAGE
	db 39, TACKLE
	db 46, THRASH
	db 53, EARTHQUAKE
	db 0 ; no more level-up moves

HitmonleeEvosAttacks:
	db 0 ; no more evolutions
	db 1, DOUBLE_KICK
	db 6, TACKLE
	db 11, JUMP_KICK
	db 16, JUMP_KICK
	db 21, FOCUS_ENERGY
	db 26, HI_JUMP_KICK
	db 31, TACKLE
	db 36, TACKLE
	db 41, ENDURE
	db 46, FAKE_OUT
	db 51, REVERSAL
	db 0 ; no more level-up moves

HitmonchanEvosAttacks:
	db 0 ; no more evolutions
	db 99, BULLET_PUNCH
	db 99, PURSUIT
	db 99, RAPID_SPIN
	db 99, CLOSE_COMBAT
	db 99, AGILITY
	db 99, THUNDERPUNCH
	db 99, FIRE_PUNCH
	db 99, DETECT
;	db 100, HI_JUMP_KICK
;	db 100, ICE_PUNCH
;	db 100, MACH_PUNCH
;	db 100, BULK_UP
	db 100, ENCORE
	db 100, NO_MOVE
	db 100, NO_MOVE
	db 100, NO_MOVE
	db 0 ; no more level-up moves

LickitungEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 7, SLUDGE_BOMB
	db 13, DEFENSE_CURL
	db 19, ZEN_HEADBUTT
	db 25, WRAP
	db 31, DISABLE
	db 37, BODY_SLAM
	db 43, SCREECH
	db 0 ; no more level-up moves

KoffingEvosAttacks:
	db EVOLVE_LEVEL, 35, WEEZING
	db 0 ; no more evolutions
	db 1, POISON_GAS
	db 0 ; no more level-up moves

WeezingEvosAttacks:
	db 0 ; no more evolutions
	db 99, CURSE
	db 99, DESTINY_BOND
	db 99, PAIN_SPLIT
	db 99, STOCKPILE
	db 99, GYRO_BALL
	db 99, EXPLOSION
	db 100, HAZE
	db 100, SLUDGE_BOMB
	db 100, TOXIC
	db 100, WILL_O_WISP
	db 0 ; no more level-up moves

RhyhornEvosAttacks:
	db EVOLVE_LEVEL, 42, RHYDON
	db 0 ; no more evolutions
	db 1, SWORDS_DANCE
	db 1, SWORDS_DANCE
	db 13, ZEN_HEADBUTT
	db 19, SWORDS_DANCE
	db 31, SCARY_FACE
	db 37, SWORDS_DANCE
	db 49, SWORDS_DANCE
	db 55, EARTHQUAKE
	db 0 ; no more level-up moves

RhydonEvosAttacks:
	db 0 ; no more evolutions
	db 1, SWORDS_DANCE
	db 1, SWORDS_DANCE
	db 1, ZEN_HEADBUTT
	db 1, SWORDS_DANCE
	db 13, ZEN_HEADBUTT
	db 19, SWORDS_DANCE
	db 31, SCARY_FACE
	db 37, SWORDS_DANCE
	db 54, SWORDS_DANCE
	db 65, EARTHQUAKE
	db 0 ; no more level-up moves

ChanseyEvosAttacks:
	db EVOLVE_HAPPINESS, TR_ANYTIME, BLISSEY
	db 0 ; no more evolutions
	db 1, POUND
	db 5, SWORDS_DANCE
	db 9, SWORDS_DANCE
	db 13, SOFTBOILED
	db 17, DOUBLESLAP
	db 23, MINIMIZE
	db 29, HYPNOSIS
	db 35, TACKLE
	db 41, DEFENSE_CURL
	db 49, LIGHT_SCREEN
	db 57, DOUBLE_EDGE
	db 0 ; no more level-up moves

TangelaEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

KangaskhanEvosAttacks:
	db 0 ; no more evolutions
	db 99, DOUBLE_EDGE
	db 99, HAMMER_ARM
	db 99, CRUNCH
	db 99, OUTRAGE
	db 100, FAKE_OUT
	db 100, SUCKER_PUNCH
	db 100, RETURN
	db 100, EARTHQUAKE
	db 0 ; no more level-up moves

RhyperiorEvosAttacks:
	db EVOLVE_LEVEL, 32, SEADRA
	db 0 ; no more evolutions
	db 1, TACKLE
	db 8, TACKLE
	db 15, CRUNCH
	db 22, HYDRO_PUMP
	db 29, OUTRAGE
	db 36, AGILITY
	db 43, HYDRO_PUMP
	db 0 ; no more level-up moves

SeadraEvosAttacks:
	db EVOLVE_TRADE, DRAGON_SCALE, KINGDRA
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, TACKLE
	db 1, CRUNCH
	db 1, HYDRO_PUMP
	db 8, TACKLE
	db 15, CRUNCH
	db 22, HYDRO_PUMP
	db 29, OUTRAGE
	db 40, AGILITY
	db 51, HYDRO_PUMP
	db 0 ; no more level-up moves

GoldeenEvosAttacks:
	db EVOLVE_LEVEL, 33, SEAKING
	db 0 ; no more evolutions
	db 1, DRILL_PECK
	db 0 ; no more level-up moves

SeakingEvosAttacks:
	db 0 ; no more evolutions
	db 99, AQUA_TAIL
	db 99, BODY_SLAM
	db 99, HAZE
	db 99, SKULL_BASH
	db 99, FLAIL
	db 99, AGILITY
	db 100, MEGAHORN
	db 100, WATERFALL
	db 100, POISON_JAB
	db 100, BODY_SLAM
	db 0 ; no more level-up moves

StaryuEvosAttacks:
	db EVOLVE_ITEM, WATER_STONE, STARMIE
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

StarmieEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, RAPID_SPIN
	db 1, RECOVER
	db 1, BUBBLEBEAM
	db 37, CONFUSE_RAY
	db 0 ; no more level-up moves

MrMimeEvosAttacks:
	db 0 ; no more evolutions
	db 1, BARRIER
	db 6, PSYWAVE
	db 11, SUBSTITUTE
	db 16, TACKLE
	db 21, DOUBLESLAP
	db 26, LIGHT_SCREEN
	db 26, REFLECT
	db 31, ENCORE
	db 36, PSYBEAM
	db 41, BATON_PASS
	db 46, SAFEGUARD
	db 0 ; no more level-up moves

ScytherEvosAttacks:
	db EVOLVE_TRADE, METAL_COAT, SCIZOR
	db 0 ; no more evolutions
	db 1, QUICK_ATTACK
	db 1, CRUNCH
	db 6, FOCUS_ENERGY
	db 12, PURSUIT
	db 18, TACKLE
	db 24, AGILITY
	db 30, WING_ATTACK
	db 36, SLASH
	db 42, SWORDS_DANCE
	db 48, CALM_MIND
	db 0 ; no more level-up moves

JynxEvosAttacks:
	db 0 ; no more evolutions
	db 1, POUND
	db 1, TACKLE
	db 1, LOVELY_KISS
	db 1, BLIZZARD
	db 9, LOVELY_KISS
	db 13, BLIZZARD
	db 21, DOUBLESLAP
	db 25, ICE_PUNCH
	db 35, MEAN_LOOK
	db 41, BODY_SLAM
	db 51, PERISH_SONG
	db 57, BLIZZARD
	db 0 ; no more level-up moves

ElectabuzzEvosAttacks:
	db 0 ; no more evolutions
	db 1, QUICK_ATTACK
	db 1, CRUNCH
	db 1, THUNDERPUNCH
	db 9, THUNDERPUNCH
	db 17, LIGHT_SCREEN
	db 25, SWIFT
	db 36, SCREECH
	db 47, THUNDERBOLT
	db 58, THUNDER
	db 0 ; no more level-up moves

MagmarEvosAttacks:
	db 0 ; no more evolutions
	db 1, FLAMETHROWER
	db 1, CRUNCH
	db 1, TACKLE
	db 1, FIRE_PUNCH
	db 7, CRUNCH
	db 13, TACKLE
	db 19, FIRE_PUNCH
	db 25, TACKLE
	db 33, SUNNY_DAY
	db 41, FLAMETHROWER
	db 49, CONFUSE_RAY
	db 57, FIRE_BLAST
	db 0 ; no more level-up moves

PinsirEvosAttacks:
	db 0 ; no more evolutions
	db 7, FOCUS_ENERGY
	db 13, BIND
	db 19, SEISMIC_TOSS
	db 25, TACKLE
	db 31, RETURN
	db 37, GIGA_DRAIN
	db 43, SWORDS_DANCE
	db 0 ; no more level-up moves

TaurosEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 4, SWORDS_DANCE
	db 8, RAGE
	db 13, SWORDS_DANCE
	db 19, SCARY_FACE
	db 26, PURSUIT
	db 34, REST
	db 43, THRASH
	db 53, SWORDS_DANCE
	db 0 ; no more level-up moves

MagikarpEvosAttacks:
	db EVOLVE_LEVEL, 20, GYARADOS
	db 0 ; no more evolutions
	db 1, TACKLE
	db 15, TACKLE
	db 30, FLAIL
	db 0 ; no more level-up moves

GyaradosEvosAttacks:
	db 0 ; no more evolutions
	db 1, THRASH
	db 20, CRUNCH
	db 25, OVERHEAT
	db 30, CRUNCH
	db 35, OUTRAGE
	db 40, HYDRO_PUMP
	db 45, RAIN_DANCE
	db 50, HYPER_BEAM
	db 0 ; no more level-up moves

LaprasEvosAttacks:
	db 0 ; no more evolutions
	db 1, HYDRO_PUMP
	db 1, SWORDS_DANCE
	db 1, HYPNOSIS
	db 8, MIST
	db 15, BODY_SLAM
	db 22, CONFUSE_RAY
	db 29, PERISH_SONG
	db 36, ICE_BEAM
	db 43, RAIN_DANCE
	db 50, SAFEGUARD
	db 57, HYDRO_PUMP
	db 0 ; no more level-up moves

DittoEvosAttacks:
	db 0 ; no more evolutions
	db 1, TRANSFORM
	db 0 ; no more level-up moves

EeveeEvosAttacks:
	db EVOLVE_ITEM, THUNDERSTONE, JOLTEON
	db EVOLVE_ITEM, WATER_STONE, VAPOREON
	db EVOLVE_ITEM, FIRE_STONE, FLAREON
	db EVOLVE_HAPPINESS, TR_MORNDAY, ESPEON
	db EVOLVE_HAPPINESS, TR_NITE, UMBREON
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SWORDS_DANCE
	db 8, GLARE
	db 16, SWORDS_DANCE
	db 23, QUICK_ATTACK
	db 30, CRUNCH
	db 36, BATON_PASS
	db 42, SWORDS_DANCE
	db 0 ; no more level-up moves

VaporeonEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SWORDS_DANCE
	db 8, GLARE
	db 16, HYDRO_PUMP
	db 23, QUICK_ATTACK
	db 30, CRUNCH
	db 36, AURORA_BEAM
	db 42, HAZE
	db 47, ACID_ARMOR
	db 52, HYDRO_PUMP
	db 0 ; no more level-up moves

JolteonEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SWORDS_DANCE
	db 8, GLARE
	db 16, THUNDERBOLT
	db 23, QUICK_ATTACK
	db 30, DOUBLE_KICK
	db 36, PIN_MISSILE
	db 42, THUNDER_WAVE
	db 47, AGILITY
	db 52, THUNDER
	db 0 ; no more level-up moves

FlareonEvosAttacks:
	db 0 ; no more evolutions
	db 99, CHARM
	db 99, DETECT
	db 99, FLAIL
	db 99, BATON_PASS
	db 100, FLARE_BLITZ
	db 100, CURSE
	db 100, RETURN
	db 100, WILL_O_WISP
	db 0 ; no more level-up moves

PorygonEvosAttacks:
	db EVOLVE_TRADE, UP_GRADE, PORYGON2
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, TACKLE
	db 1, TACKLE
	db 9, AGILITY
	db 12, PSYBEAM
	db 20, RECOVER
	db 24, CALM_MIND
	db 32, TACKLE
	db 36, TRI_ATTACK
	db 44, ZAP_CANNON
	db 0 ; no more level-up moves

OmanyteEvosAttacks:
	db EVOLVE_LEVEL, 40, OMASTAR
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, TACKLE
	db 13, CRUNCH
	db 19, HYDRO_PUMP
	db 31, CRUNCH
	db 37, PROTECT
	db 49, ANCIENTPOWER
	db 55, HYDRO_PUMP
	db 0 ; no more level-up moves

OmastarEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, TACKLE
	db 1, CRUNCH
	db 13, CRUNCH
	db 19, HYDRO_PUMP
	db 31, CRUNCH
	db 37, PROTECT
	db 40, TACKLE
	db 54, ANCIENTPOWER
	db 65, HYDRO_PUMP
	db 0 ; no more level-up moves

KabutoEvosAttacks:
	db EVOLVE_LEVEL, 40, KABUTOPS
	db 0 ; no more evolutions
	db 1, SCRATCH
	db 1, TACKLE
	db 10, ENERGY_BALL
	db 19, CRUNCH
	db 28, GLARE
	db 37, ENDURE
	db 46, GIGA_DRAIN
	db 55, ANCIENTPOWER
	db 0 ; no more level-up moves

KabutopsEvosAttacks:
	db 0 ; no more evolutions
	db 1, SCRATCH
	db 1, TACKLE
	db 1, ENERGY_BALL
	db 10, ENERGY_BALL
	db 19, CRUNCH
	db 28, GLARE
	db 37, ENDURE
	db 40, SLASH
	db 51, GIGA_DRAIN
	db 65, ANCIENTPOWER
	db 0 ; no more level-up moves

AerodactylEvosAttacks:
	db 0 ; no more evolutions
	db 1, WING_ATTACK
	db 8, AGILITY
	db 15, CRUNCH
	db 22, SLUDGE_BOMB
	db 29, ANCIENTPOWER
	db 36, SCARY_FACE
	db 43, SWORDS_DANCE
	db 50, HYPER_BEAM
	db 0 ; no more level-up moves

SnorlaxEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 8, AMNESIA
	db 15, DEFENSE_CURL
	db 22, BELLY_DRUM
	db 29, DRAGON_CLAW
	db 36, SNORE
	db 36, REST
	db 43, BODY_SLAM
	db 50, ROLLOUT
	db 57, HYPER_BEAM
	db 0 ; no more level-up moves

ArticunoEvosAttacks:
	db 0 ; no more evolutions
	db 1, HURRICANE
	db 1, BLIZZARD
	db 13, MIST
	db 25, AGILITY
	db 37, TACKLE
	db 49, ICE_BEAM
	db 61, REFLECT
	db 73, BLIZZARD
	db 0 ; no more level-up moves

ZapdosEvosAttacks:
	db 0 ; no more evolutions
	db 1, DRILL_PECK
	db 1, THUNDERBOLT
	db 13, THUNDER_WAVE
	db 25, AGILITY
	db 37, DETECT
	db 49, DRILL_PECK
	db 61, LIGHT_SCREEN
	db 73, THUNDER
	db 0 ; no more level-up moves

MoltresEvosAttacks:
	db 0 ; no more evolutions
	db 1, WING_ATTACK
	db 1, FLAMETHROWER
	db 13, FIRE_SPIN
	db 25, AGILITY
	db 37, ENDURE
	db 49, FLAMETHROWER
	db 61, SAFEGUARD
	db 73, SKY_ATTACK
	db 0 ; no more level-up moves

DratiniEvosAttacks:
	db EVOLVE_LEVEL, 30, DRAGONAIR
	db 0 ; no more evolutions
	db 1, WRAP
	db 0 ; no more level-up moves

DragonairEvosAttacks:
	db EVOLVE_LEVEL, 55, DRAGONITE
	db 0 ; no more evolutions
	db 1, WRAP
	db 0 ; no more level-up moves

DragoniteEvosAttacks:
	db 0 ; no more evolutions
	db 99, AQUA_JET
	db 99, EXTREMESPEED
	db 99, IRON_TAIL
	db 99, FIRE_PUNCH
	db 99, THUNDERPUNCH
	db 99, THUNDER_WAVE
	db 99, AGILITY
	db 99, DRAGON_TAIL
	db 99, AQUA_TAIL
	db 100, DRAGON_DANCE
	db 100, OUTRAGE
	db 100, IRON_HEAD
	db 100, ROOST
	db 0 ; no more level-up moves

MewtwoEvosAttacks:
	db 0 ; no more evolutions
	db 99, ENCORE
	db 99, WILD_CHARGE
	db 99, STEALTH_ROCK
	db 100, CALM_MIND
	db 100, AURA_SPHERE
	db 100, PSYCHIC_M
	db 100, SIGNAL_BEAM
	db 0 ; no more level-up moves

MewEvosAttacks:
	db 0 ; no more evolutions
	db 1, POUND
	db 10, TRANSFORM
	db 20, MEGA_PUNCH
	db 30, METRONOME
	db 40, PSYCHIC_M
	db 50, ANCIENTPOWER
	db 0 ; no more level-up moves

ChikoritaEvosAttacks:
	db EVOLVE_LEVEL, 16, BAYLEEF
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SWORDS_DANCE
	db 8, GIGA_DRAIN
	db 12, REFLECT
	db 15, POISONPOWDER
	db 22, SYNTHESIS
	db 29, BODY_SLAM
	db 36, LIGHT_SCREEN
	db 43, SAFEGUARD
	db 50, SOLARBEAM
	db 0 ; no more level-up moves

BayleefEvosAttacks:
	db EVOLVE_LEVEL, 32, MEGANIUM
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SWORDS_DANCE
	db 1, GIGA_DRAIN
	db 1, REFLECT
	db 8, GIGA_DRAIN
	db 12, REFLECT
	db 15, POISONPOWDER
	db 23, SYNTHESIS
	db 31, BODY_SLAM
	db 39, LIGHT_SCREEN
	db 47, SAFEGUARD
	db 55, SOLARBEAM
	db 0 ; no more level-up moves

MeganiumEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SWORDS_DANCE
	db 1, GIGA_DRAIN
	db 1, REFLECT
	db 8, GIGA_DRAIN
	db 12, REFLECT
	db 15, POISONPOWDER
	db 23, SYNTHESIS
	db 31, BODY_SLAM
	db 41, LIGHT_SCREEN
	db 51, SAFEGUARD
	db 61, SOLARBEAM
	db 0 ; no more level-up moves

CyndaquilEvosAttacks:
	db EVOLVE_LEVEL, 14, QUILAVA
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, CRUNCH
	db 6, TACKLE
	db 12, FLAMETHROWER
	db 19, QUICK_ATTACK
	db 27, TACKLE
	db 36, SWIFT
	db 46, FLAMETHROWER
	db 0 ; no more level-up moves

QuilavaEvosAttacks:
	db EVOLVE_LEVEL, 36, TYPHLOSION
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, CRUNCH
	db 1, TACKLE
	db 6, TACKLE
	db 12, FLAMETHROWER
	db 21, QUICK_ATTACK
	db 31, TACKLE
	db 42, SWIFT
	db 54, FLAMETHROWER
	db 0 ; no more level-up moves

TyphlosionEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, CRUNCH
	db 1, TACKLE
	db 1, FLAMETHROWER
	db 6, TACKLE
	db 12, FLAMETHROWER
	db 21, QUICK_ATTACK
	db 31, TACKLE
	db 45, SWIFT
	db 60, FLAMETHROWER
	db 0 ; no more level-up moves

TotodileEvosAttacks:
	db EVOLVE_LEVEL, 18, CROCONAW
	db 0 ; no more evolutions
	db 1, SCRATCH
	db 1, CRUNCH
	db 7, RAGE
	db 13, HYDRO_PUMP
	db 20, CRUNCH
	db 27, SCARY_FACE
	db 35, SLASH
	db 43, SCREECH
	db 52, HYDRO_PUMP
	db 0 ; no more level-up moves

CroconawEvosAttacks:
	db EVOLVE_LEVEL, 30, FERALIGATR
	db 0 ; no more evolutions
	db 1, SCRATCH
	db 0 ; no more level-up moves

FeraligatrEvosAttacks:
	db 0 ; no more evolutions
	db 99, AQUA_JET
	db 99, ICE_PUNCH
	db 99, AGILITY
	db 99, FLAIL
	db 99, AQUA_TAIL
	db 100, DRAGON_DANCE
	db 100, LIQUIDATION
	db 100, CRUNCH
	db 100, EARTHQUAKE
	db 0 ; no more level-up moves

ElectivireEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 5, DEFENSE_CURL
	db 11, QUICK_ATTACK
	db 17, TACKLE
	db 25, BODY_SLAM
	db 33, REST
	db 41, AMNESIA
	db 0 ; no more level-up moves

FurretEvosAttacks:
	db 0 ; no more evolutions
	db 99, CHARM
	db 99, DOUBLE_EDGE
	db 99, PURSUIT
	db 99, AGILITY
	db 99, QUICK_ATTACK
	db 99, REST
	db 99, AMNESIA
	db 99, BATON_PASS
	db 100, COIL
	db 100, RETURN
	db 100, IRON_TAIL
	db 100, SUCKER_PUNCH
	db 0 ; no more level-up moves

HoothootEvosAttacks:
	db EVOLVE_LEVEL, 20, NOCTOWL
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

NoctowlEvosAttacks:
	db 0 ; no more evolutions
	db 99, AGILITY
	db 99, HURRICANE
	db 99, NIGHT_SHADE
	db 99, WHIRLWIND
	db 99, HYPNOSIS
	db 99, DREAM_EATER
	db 99, EXTRASENSORY
	db 100, HYPER_VOICE
	db 100, ROOST
	db 100, MOONBLAST
	db 100, AIR_SLASH
	db 0 ; no more level-up moves

PorygonZEvosAttacks:
	db 0 ; no more evolutions
	db 99, NASTY_PLOT
	db 99, RECOVER
	db 99, SIGNAL_BEAM
	db 99, HYPER_BEAM
	db 100, THUNDERBOLT
	db 100, TRI_ATTACK
	db 100, NASTY_PLOT
	db 100, PSYCHIC_M
	db 0 ; no more level-up moves

LedianEvosAttacks:
	db 0 ; no more evolutions
	db 99, PSYBEAM
	db 99, SWIFT
	db 99, MACH_PUNCH
	db 99, BATON_PASS
	db 100, LIGHT_SCREEN
	db 100, BUG_BUZZ
	db 100, AIR_SLASH
	db 100, REFLECT
	db 0 ; no more level-up moves

WeavileEvosAttacks:
	db 0 ; no more evolutions
	db 1, POISON_JAB
	db 1, GLARE
	db 6, SCARY_FACE
	db 11, TACKLE
	db 17, NIGHT_SHADE
	db 23, LEECH_LIFE
	db 30, TACKLE
	db 37, SPIDER_WEB
	db 45, AGILITY
	db 53, PSYCHIC_M
	db 0 ; no more level-up moves

AriadosEvosAttacks:
	db 0 ; no more evolutions
	db 99, MEGAHORN
	db 99, NIGHT_SLASH
	db 99, PURSUIT
	db 99, INFESTATION
	db 99, NIGHT_SHADE
	db 99, SHADOW_SNEAK
	db 99, SPIDER_WEB
	db 99, AGILITY
	db 100, POISON_JAB
	db 100, LEECH_LIFE
	db 100, SWORDS_DANCE
	db 100, SUCKER_PUNCH
	db 0 ; no more level-up moves

CrobatEvosAttacks:
	db 0 ; no more evolutions
	db 1, SCREECH
	db 1, LEECH_LIFE
	db 1, SLUDGE_BOMB
	db 6, SLUDGE_BOMB
	db 12, CRUNCH
	db 19, CONFUSE_RAY
	db 30, WING_ATTACK
	db 42, MEAN_LOOK
	db 55, HAZE
	db 0 ; no more level-up moves

ChinchouEvosAttacks:
	db EVOLVE_LEVEL, 27, LANTURN
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, THUNDER_WAVE
	db 5, SLUDGE_BOMB
	db 13, FLAIL
	db 17, HYDRO_PUMP
	db 25, DRILL_RUN
	db 29, CONFUSE_RAY
	db 37, SWORDS_DANCE
	db 41, HYDRO_PUMP
	db 0 ; no more level-up moves

LanturnEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, THUNDER_WAVE
	db 1, SLUDGE_BOMB
	db 5, SLUDGE_BOMB
	db 13, FLAIL
	db 17, HYDRO_PUMP
	db 25, DRILL_RUN
	db 33, CONFUSE_RAY
	db 45, SWORDS_DANCE
	db 53, HYDRO_PUMP
	db 0 ; no more level-up moves

PichuEvosAttacks:
	db EVOLVE_HAPPINESS, TR_ANYTIME, PIKACHU
	db 0 ; no more evolutions
	db 1, THUNDERBOLT
	db 1, CHARM
	db 6, SWORDS_DANCE
	db 8, THUNDER_WAVE
	db 11, TACKLE
	db 0 ; no more level-up moves

CleffaEvosAttacks:
	db EVOLVE_HAPPINESS, TR_ANYTIME, CLEFAIRY
	db 0 ; no more evolutions
	db 1, POUND
	db 1, CHARM
	db 4, ENCORE
	db 8, HYPNOSIS
	db 13, TACKLE
	db 0 ; no more level-up moves

IgglybuffEvosAttacks:
	db EVOLVE_HAPPINESS, TR_ANYTIME, JIGGLYPUFF
	db 0 ; no more evolutions
	db 1, HYPNOSIS
	db 1, CHARM
	db 4, DEFENSE_CURL
	db 9, POUND
	db 14, TACKLE
	db 0 ; no more level-up moves

TogepiEvosAttacks:
	db EVOLVE_HAPPINESS, TR_ANYTIME, TOGETIC
	db 0 ; no more evolutions
	db 1, SWORDS_DANCE
	db 0 ; no more level-up moves

TogeticEvosAttacks:
	db 0 ; no more evolutions
	db 1, SWORDS_DANCE
	db 0 ; no more level-up moves

AmbipomEvosAttacks:
	db 0 ; no more evolutions
	db 99, AGILITY
	db 99, PURSUIT
	db 99, DUAL_CHOP
	db 99, AGILITY
	db 100, RETURN
	db 100, ACROBATICS
	db 100, U_TURN
	db 100, BRICK_BREAK
	db 0 ; no more level-up moves

XatuEvosAttacks:
	db 0 ; no more evolutions
	db 99, HAZE
	db 99, QUICK_ATTACK
	db 99, ROOST
	db 99, SUCKER_PUNCH
	db 99, AIR_SLASH
	db 99, NIGHT_SHADE
	db 99, CONFUSE_RAY
	db 99, FUTURE_SIGHT
	db 100, PSYCHIC_M
	db 100, SHADOW_BALL
	db 100, CALM_MIND
	db 100, THUNDER_WAVE
	db 0 ; no more level-up moves

TangrowthEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SWORDS_DANCE
	db 9, THUNDERBOLT
	db 16, THUNDER_WAVE
	db 23, CURSE
	db 30, LIGHT_SCREEN
	db 37, THUNDER
	db 0 ; no more level-up moves

FlaaffyEvosAttacks:
	db EVOLVE_LEVEL, 30, AMPHAROS
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SWORDS_DANCE
	db 1, THUNDERBOLT
	db 9, THUNDERBOLT
	db 18, THUNDER_WAVE
	db 27, CURSE
	db 36, LIGHT_SCREEN
	db 45, THUNDER
	db 0 ; no more level-up moves

AmpharosEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SWORDS_DANCE
	db 1, THUNDERBOLT
	db 1, THUNDER_WAVE
	db 9, THUNDERBOLT
	db 18, THUNDER_WAVE
	db 27, CURSE
	db 30, THUNDERPUNCH
	db 42, LIGHT_SCREEN
	db 57, THUNDER
	db 0 ; no more level-up moves

BellossomEvosAttacks:
	db 0 ; no more evolutions
	db 99, GROWTH
	db 99, POISONPOWDER
	db 99, SLEEP_POWDER
	db 99, GIGA_DRAIN
	db 99, STUN_SPORE
	db 99, SUNNY_DAY
	db 99, PETAL_DANCE
	db 100, QUIVER_DANCE
	db 100, ENERGY_BALL
	db 100, SYNTHESIS
	db 100, MOONBLAST
	db 0 ; no more level-up moves

MarillEvosAttacks:
	db EVOLVE_LEVEL, 18, AZUMARILL
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

AzumarillEvosAttacks:
	db 0 ; no more evolutions
	db 99, AMNESIA
	db 99, BODY_SLAM
	db 99, ENCORE
	db 99, ROLLOUT
	db 99, AQUA_TAIL
	db 99, RAIN_DANCE
	db 99, DOUBLE_EDGE
	db 100, PLAY_ROUGH
	db 100, BELLY_DRUM
	db 100, AQUA_JET
	db 100, BRICK_BREAK
	db 0 ; no more level-up moves

SudowoodoEvosAttacks:
	db 0 ; no more evolutions
	db 99, SELFDESTRUCT
	db 99, LOW_KICK
	db 99, ROCK_SLIDE
	db 100, HAMMER_ARM
	db 100, STONE_EDGE
	db 100, CURSE
	db 100, SUCKER_PUNCH
	db 0 ; no more level-up moves

PolitoedEvosAttacks:
	db 0 ; no more evolutions
	db 1, HYDRO_PUMP
	db 1, HYPNOSIS
	db 1, DOUBLESLAP
	db 1, PERISH_SONG
	db 35, PERISH_SONG
	db 51, SWAGGER
	db 0 ; no more level-up moves

MagnezoneEvosAttacks:
	db 0 ; no more evolutions
	db 100, THUNDER
	db 0 ; no more level-up moves

SkiploomEvosAttacks:
	db EVOLVE_LEVEL, 27, JUMPLUFF
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SYNTHESIS
	db 1, SWORDS_DANCE
	db 1, TACKLE
	db 5, SYNTHESIS
	db 5, SWORDS_DANCE
	db 10, TACKLE
	db 13, POISONPOWDER
	db 15, STUN_SPORE
	db 17, SLEEP_POWDER
	db 22, LEECH_SEED
	db 29, CURSE
	db 36, GIGA_DRAIN
	db 0 ; no more level-up moves

JumpluffEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SYNTHESIS
	db 1, SWORDS_DANCE
	db 1, TACKLE
	db 5, SYNTHESIS
	db 5, SWORDS_DANCE
	db 10, TACKLE
	db 13, POISONPOWDER
	db 15, STUN_SPORE
	db 17, SLEEP_POWDER
	db 22, LEECH_SEED
	db 33, CURSE
	db 44, GIGA_DRAIN
	db 0 ; no more level-up moves

AipomEvosAttacks:
	db 0 ; no more evolutions
	db 1, SCRATCH
	db 1, SWORDS_DANCE
	db 6, GLARE
	db 12, BATON_PASS
	db 19, TACKLE
	db 27, SWIFT
	db 36, SCREECH
	db 46, AGILITY
	db 0 ; no more level-up moves

TogekissEvosAttacks:
	db 0 ; no more evolutions
	db 1, ENERGY_BALL
	db 4, GROWTH
	db 10, GIGA_DRAIN
	db 19, SUNNY_DAY
	db 31, SYNTHESIS
	db 46, GIGA_DRAIN
	db 0 ; no more level-up moves

SunfloraEvosAttacks:
	db 0 ; no more evolutions
	db 99, ENCORE
	db 99, GROWTH
	db 99, GIGA_DRAIN
	db 99, PETAL_DANCE
	db 99, SOLARBEAM
	db 99, SUNNY_DAY
	db 100, ENERGY_BALL
	db 100, EARTH_POWER
	db 100, LEECH_SEED
	db 100, SYNTHESIS
	db 0 ; no more level-up moves

YanmaEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, TACKLE
	db 7, QUICK_ATTACK
	db 13, CALM_MIND
	db 19, SLUDGE_BOMB
	db 25, DETECT
	db 31, SLUDGE_BOMB
	db 37, WING_ATTACK
	db 43, SCREECH
	db 0 ; no more level-up moves

WooperEvosAttacks:
	db EVOLVE_LEVEL, 20, QUAGSIRE
	db 0 ; no more evolutions
	db 1, HYDRO_PUMP
	db 1, SWORDS_DANCE
	db 11, BODY_SLAM
	db 21, AMNESIA
	db 31, EARTHQUAKE
	db 41, RAIN_DANCE
	db 51, MIST
	db 51, HAZE
	db 0 ; no more level-up moves

QuagsireEvosAttacks:
	db 0 ; no more evolutions
	db 1, HYDRO_PUMP
	db 1, SWORDS_DANCE
	db 11, BODY_SLAM
	db 23, AMNESIA
	db 35, EARTHQUAKE
	db 47, RAIN_DANCE
	db 59, MIST
	db 59, HAZE
	db 0 ; no more level-up moves

EspeonEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SWORDS_DANCE
	db 8, GLARE
	db 16, PSYWAVE
	db 23, QUICK_ATTACK
	db 30, SWIFT
	db 36, PSYBEAM
	db 42, PSYWAVE
	db 47, PSYCHIC_M
	db 52, MORNING_SUN
	db 0 ; no more level-up moves

UmbreonEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SWORDS_DANCE
	db 8, GLARE
	db 16, PURSUIT
	db 23, QUICK_ATTACK
	db 30, CONFUSE_RAY
	db 36, FAINT_ATTACK
	db 42, MEAN_LOOK
	db 47, SCREECH
	db 52, MOONLIGHT
	db 0 ; no more level-up moves

MurkrowEvosAttacks:
	db 0 ; no more evolutions
	db 1, DRILL_PECK
	db 11, PURSUIT
	db 16, HAZE
	db 26, NIGHT_SHADE
	db 31, FAINT_ATTACK
	db 41, MEAN_LOOK
	db 0 ; no more level-up moves

SlowkingEvosAttacks:
	db 0 ; no more evolutions
	db 1, CURSE
	db 1, TACKLE
	db 6, SWORDS_DANCE
	db 15, HYDRO_PUMP
	db 20, PSYWAVE
	db 29, DISABLE
	db 34, DRAGON_CLAW
	db 43, SWAGGER
	db 48, PSYCHIC_M
	db 0 ; no more level-up moves

MisdreavusEvosAttacks:
	db 0 ; no more evolutions
	db 1, SWORDS_DANCE
	db 1, PSYWAVE
	db 6, CURSE
	db 12, CONFUSE_RAY
	db 19, MEAN_LOOK
	db 27, PSYBEAM
	db 36, PAIN_SPLIT
	db 46, PERISH_SONG
	db 0 ; no more level-up moves

UnownEvosAttacks:
	db 0 ; no more evolutions
	db 1, HIDDEN_POWER
	db 0 ; no more level-up moves

WobbuffetEvosAttacks:
	db 0 ; no more evolutions
	db 99, ENCORE
	db 99, SAFEGUARD
	db 100, COUNTER
	db 100, MIRROR_COAT
	db 100, CHARM
	db 100, DESTINY_BOND
	db 0 ; no more level-up moves

GirafarigEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SWORDS_DANCE
	db 1, PSYWAVE
	db 1, ZEN_HEADBUTT
	db 7, PSYWAVE
	db 13, ZEN_HEADBUTT
	db 20, AGILITY
	db 30, BATON_PASS
	db 41, PSYBEAM
	db 54, CRUNCH
	db 0 ; no more level-up moves

PinecoEvosAttacks:
	db EVOLVE_LEVEL, 31, FORRETRESS
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

ForretressEvosAttacks:
	db 0 ; no more evolutions
	db 100, SPIKES
	db 0 ; no more level-up moves

DunsparceEvosAttacks:
	db 0 ; no more evolutions
	db 99, AGILITY
	db 99, CURSE
	db 99, SLEEP_TALK
	db 99, PURSUIT
	db 99, ROOST
	db 99, GLARE
	db 99, DOUBLE_EDGE
	db 100, COIL
	db 100, ROLLOUT
	db 100, DRILL_RUN
	db 100, BODY_SLAM
	db 0 ; no more level-up moves

GligarEvosAttacks:
	db 0 ; no more evolutions
	db 1, POISON_JAB
	db 0 ; no more level-up moves

SteelixEvosAttacks:
	db 0 ; no more evolutions
	db 100, CURSE
	db 100, GYRO_BALL
	db 100, EARTHQUAKE
	db 0 ; no more level-up moves

SnubbullEvosAttacks:
	db EVOLVE_LEVEL, 23, GRANBULL
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SCARY_FACE
	db 4, SWORDS_DANCE
	db 8, CHARM
	db 13, CRUNCH
	db 19, TACKLE
	db 26, ROAR
	db 34, RAGE
	db 43, SWORDS_DANCE
	db 0 ; no more level-up moves

GranbullEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, SCARY_FACE
	db 4, SWORDS_DANCE
	db 8, CHARM
	db 13, CRUNCH
	db 19, TACKLE
	db 28, ROAR
	db 38, RAGE
	db 51, SWORDS_DANCE
	db 0 ; no more level-up moves

QwilfishEvosAttacks:
	db 0 ; no more evolutions
	db 1, SPIKES
	db 1, TACKLE
	db 1, POISON_JAB
	db 10, TACKLE
	db 10, MINIMIZE
	db 19, HYDRO_PUMP
	db 28, PIN_MISSILE
	db 37, SWORDS_DANCE
	db 46, HYDRO_PUMP
	db 0 ; no more level-up moves

ScizorEvosAttacks:
	db 0 ; no more evolutions
	db 1, QUICK_ATTACK
	db 1, CRUNCH
	db 6, FOCUS_ENERGY
	db 12, PURSUIT
	db 18, TACKLE
	db 24, AGILITY
	db 30, ROOST
	db 36, SLASH
	db 42, SWORDS_DANCE
	db 48, CALM_MIND
	db 0 ; no more level-up moves

ShuckleEvosAttacks:
	db 0 ; no more evolutions
	db 99, ENCORE
	db 99, WRAP
	db 100, TOXIC
	db 100, INFESTATION
	db 100, REST
	db 100, PROTECT
	db 0 ; no more level-up moves

HeracrossEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, CRUNCH
	db 6, SWORDS_DANCE
	db 12, ENDURE
	db 19, SWORDS_DANCE
	db 27, COUNTER
	db 35, SWORDS_DANCE
	db 44, REVERSAL
	db 54, MEGAHORN
	db 0 ; no more level-up moves

SneaselEvosAttacks:
	db 0 ; no more evolutions
	db 1, SCRATCH
	db 1, CRUNCH
	db 9, QUICK_ATTACK
	db 17, SCREECH
	db 25, FAINT_ATTACK
	db 33, TACKLE
	db 41, AGILITY
	db 49, SLASH
	db 65, ROOST
	db 0 ; no more level-up moves

TeddiursaEvosAttacks:
	db EVOLVE_LEVEL, 30, URSARING
	db 0 ; no more evolutions
	db 1, SCRATCH
	db 1, CRUNCH
	db 8, TACKLE
	db 15, TACKLE
	db 22, FAINT_ATTACK
	db 29, REST
	db 36, SLASH
	db 43, SNORE
	db 50, THRASH
	db 0 ; no more level-up moves

UrsaringEvosAttacks:
	db 0 ; no more evolutions
	db 1, SCRATCH
	db 1, CRUNCH
	db 1, TACKLE
	db 1, TACKLE
	db 8, TACKLE
	db 15, TACKLE
	db 22, FAINT_ATTACK
	db 29, REST
	db 39, SLASH
	db 49, SNORE
	db 59, THRASH
	db 0 ; no more level-up moves

SlugmaEvosAttacks:
	db EVOLVE_LEVEL, 38, MAGCARGO
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

MagcargoEvosAttacks:
	db 0 ; no more evolutions
	db 99, ACID_ARMOR
	db 99, ANCIENTPOWER
	db 99, AMNESIA
	db 100, FLAMETHROWER
	db 100, EARTH_POWER
	db 100, STOCKPILE
	db 100, RECOVER
	db 0 ; no more level-up moves

MismagiusEvosAttacks:
	db EVOLVE_LEVEL, 33, PILOSWINE
	db 0 ; no more evolutions
	db 99, CURSE
	db 99, DESTINY_BOND
	db 99, NASTY_PLOT
	db 99, SHADOW_SNEAK
	db 99, SUCKER_PUNCH
	db 100, SHADOW_BALL
	db 100, POWER_GEM
	db 100, CALM_MIND
	db 100, DAZLINGGLEAM
	db 0 ; no more level-up moves

PiloswineEvosAttacks:
	db 0 ; no more evolutions
	db 1, SWORDS_DANCE
	db 1, BLIZZARD
	db 1, ENDURE
	db 10, BLIZZARD
	db 19, ENDURE
	db 28, SWORDS_DANCE
	db 33, SWORDS_DANCE
	db 42, MIST
	db 56, BLIZZARD
	db 70, AMNESIA
	db 0 ; no more level-up moves

CorsolaEvosAttacks:
	db 0 ; no more evolutions
	db 99, AMNESIA
	db 99, BARRIER
	db 99, CONFUSE_RAY
	db 99, EARTH_POWER
	db 100, POWER_GEM
	db 100, TOXIC
	db 100, RECOVER
	db 100, SCALD
	db 0 ; no more level-up moves

RemoraidEvosAttacks:
	db EVOLVE_LEVEL, 25, OCTILLERY
	db 0 ; no more evolutions
	db 1, HYDRO_PUMP
	db 0 ; no more level-up moves

OctilleryEvosAttacks:
	db 0 ; no more evolutions
	db 99, FLAIL
	db 99, HAZE
	db 99, SCREECH
	db 99, PSYBEAM
	db 99, SIGNAL_BEAM
	db 99, HYPER_BEAM
	db 100, HYDRO_PUMP
	db 100, ICE_BEAM
	db 100, GUNK_SHOT
	db 100, ACID_SPRAY
	db 0 ; no more level-up moves

DelibirdEvosAttacks:
	db 0 ; no more evolutions
	db 99, DESTINY_BOND
	db 99, FREEZE_DRY
	db 99, FUTURE_SIGHT
	db 99, ICE_SHARD
	db 99, RAPID_SPIN
	db 99, PRESENT
	db 100, DRILL_PECK
	db 100, BLIZZARD
	db 100, ICY_WIND
	db 100, SPIKES
	db 0 ; no more level-up moves

MantineEvosAttacks:
	db 0 ; no more evolutions
	db 99, HAZE
	db 99, MIRROR_COAT
	db 99, PSYBEAM
	db 99, SIGNAL_BEAM
	db 99, AGILITY
	db 100, HYDRO_PUMP
	db 100, ROOST
	db 100, AMNESIA
	db 100, AIR_SLASH
	db 0 ; no more level-up moves

SkarmoryEvosAttacks:
	db 0 ; no more evolutions
	db 99, BRAVE_BIRD
	db 99, CURSE
	db 99, DRILL_PECK
	db 99, PURSUIT
	db 99, AGILITY
	db 99, STEEL_WING
	db 99, NIGHT_SLASH
	db 100, WHIRLWIND
	db 100, ROOST
	db 100, SPIKES
	db 100, TOXIC
	db 0 ; no more level-up moves

MagmortarEvosAttacks:
	db 0 ; no more evolutions
	db 1, CRUNCH
	db 1, FLAMETHROWER
	db 7, ROAR
	db 13, TACKLE
	db 20, CRUNCH
	db 27, FAINT_ATTACK
	db 35, FLAMETHROWER
	db 43, CRUNCH
	db 0 ; no more level-up moves

HoundoomEvosAttacks:
	db 0 ; no more evolutions
	db 1, CRUNCH
	db 1, FLAMETHROWER
	db 7, ROAR
	db 13, TACKLE
	db 20, CRUNCH
	db 30, FAINT_ATTACK
	db 41, FLAMETHROWER
	db 52, CRUNCH
	db 0 ; no more level-up moves

KingdraEvosAttacks:
	db 0 ; no more evolutions
	db 99, FLAIL
	db 99, FOCUS_ENERGY
	db 99, AGILITY
	db 100, DRAGON_DANCE
	db 100, OUTRAGE
	db 100, WATERFALL
	db 100, IRON_HEAD
	db 0 ; no more level-up moves

PhanpyEvosAttacks:
	db EVOLVE_LEVEL, 25, DONPHAN
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

DonphanEvosAttacks:
	db 0 ; no more evolutions
	db 1, SWORDS_DANCE
	db 1, SWORDS_DANCE
	db 9, DEFENSE_CURL
	db 17, FLAIL
	db 25, SWORDS_DANCE
	db 33, ROLLOUT
	db 41, RAPID_SPIN
	db 49, EARTHQUAKE
	db 0 ; no more level-up moves

Porygon2EvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 1, TACKLE
	db 1, TACKLE
	db 9, AGILITY
	db 12, PSYBEAM
	db 20, RECOVER
	db 24, DEFENSE_CURL
	db 32, TACKLE
	db 36, TRI_ATTACK
	db 44, ZAP_CANNON
	db 0 ; no more level-up moves

StantlerEvosAttacks:
	db 0 ; no more evolutions
	db 99, DOUBLE_KICK
	db 99, MEGAHORN
	db 99, HYPNOSIS
	db 99, CONFUSE_RAY
	db 100, JUMP_KICK
	db 100, ZEN_HEADBUTT
	db 100, RETURN
	db 100, ROAR
	db 0 ; no more level-up moves

SmeargleEvosAttacks:
	db 0 ; no more evolutions
	db 1, SKETCH
	db 11, SKETCH
	db 21, SKETCH
	db 31, SKETCH
	db 41, SKETCH
	db 51, SKETCH
	db 61, SKETCH
	db 71, SKETCH
	db 81, SKETCH
	db 91, SKETCH
	db 0 ; no more level-up moves

TyrogueEvosAttacks:
	db EVOLVE_STAT, 20, ATK_LT_DEF, HITMONCHAN
	db EVOLVE_STAT, 20, ATK_GT_DEF, HITMONLEE
	db EVOLVE_STAT, 20, ATK_EQ_DEF, HITMONTOP
	db 0 ; no more evolutions
	db 1, TACKLE
	db 0 ; no more level-up moves

HitmontopEvosAttacks:
	db 0 ; no more evolutions
	db 99, MACH_PUNCH
	db 99, PURSUIT
	db 99, RAPID_SPIN
	db 99, CLOSE_COMBAT
	db 99, FAKE_OUT
	db 99, DETECT
	db 99, QUICK_ATTACK
	db 99, AGILITY
	db 99, GYRO_BALL
	db 100, HI_JUMP_KICK
	db 100, ROCK_SLIDE
	db 100, BULK_UP
	db 100, BULLET_PUNCH
	db 0 ; no more level-up moves

SmoochumEvosAttacks:
	db EVOLVE_LEVEL, 30, JYNX
	db 0 ; no more evolutions
	db 1, POUND
	db 0 ; no more level-up moves

ElekidEvosAttacks:
	db EVOLVE_LEVEL, 30, ELECTABUZZ
	db 0 ; no more evolutions
	db 1, QUICK_ATTACK
	db 0 ; no more level-up moves

MagbyEvosAttacks:
	db EVOLVE_LEVEL, 30, MAGMAR
	db 0 ; no more evolutions
	db 1, FLAMETHROWER
	db 0 ; no more level-up moves

MiltankEvosAttacks:
	db 0 ; no more evolutions
	db 1, TACKLE
	db 4, SWORDS_DANCE
	db 8, DEFENSE_CURL
	db 13, ZEN_HEADBUTT
	db 19, MILK_DRINK
	db 26, BIDE
	db 34, ROLLOUT
	db 43, BODY_SLAM
	db 53, HEAL_BELL
	db 0 ; no more level-up moves

BlisseyEvosAttacks:
	db 0 ; no more evolutions
	db 1, POUND
	db 4, SWORDS_DANCE
	db 7, SWORDS_DANCE
	db 10, SOFTBOILED
	db 13, DOUBLESLAP
	db 18, MINIMIZE
	db 23, HYPNOSIS
	db 28, TACKLE
	db 33, DEFENSE_CURL
	db 40, LIGHT_SCREEN
	db 47, DOUBLE_EDGE
	db 0 ; no more level-up moves

RaikouEvosAttacks:
	db 0 ; no more evolutions
	db 1, CRUNCH
	db 1, CRUNCH
	db 11, THUNDERBOLT
	db 21, ROAR
	db 31, QUICK_ATTACK
	db 41, DRILL_RUN
	db 51, REFLECT
	db 61, CRUNCH
	db 71, THUNDER
	db 0 ; no more level-up moves

EnteiEvosAttacks:
	db 0 ; no more evolutions
	db 1, CRUNCH
	db 1, CRUNCH
	db 11, FLAMETHROWER
	db 21, ROAR
	db 31, FIRE_SPIN
	db 41, ZEN_HEADBUTT
	db 51, FLAMETHROWER
	db 61, SWAGGER
	db 71, FIRE_BLAST
	db 0 ; no more level-up moves

SuicuneEvosAttacks:
	db 0 ; no more evolutions
	db 1, CRUNCH
	db 1, CRUNCH
	db 11, BUBBLEBEAM
	db 21, RAIN_DANCE
	db 31, HURRICANE
	db 41, AURORA_BEAM
	db 51, MIST
	db 61, MIRROR_COAT
	db 71, HYDRO_PUMP
	db 0 ; no more level-up moves

LarvitarEvosAttacks:
	db EVOLVE_LEVEL, 30, PUPITAR
	db 0 ; no more evolutions
	db 1, CRUNCH
	db 0 ; no more level-up moves

PupitarEvosAttacks:
	db EVOLVE_LEVEL, 55, TYRANITAR
	db 0 ; no more evolutions
	db 1, CRUNCH
	db 0 ; no more level-up moves

TyranitarEvosAttacks:
	db 0 ; no more evolutions
	db 99, CURSE
	db 99, IRON_DEFENSE
	db 99, IRON_HEAD
	db 99, OUTRAGE
	db 99, PURSUIT
	db 99, SANDSTORM
	db 99, EARTHQUAKE
	db 100, CRUNCH
	db 100, DRAGON_DANCE
	db 100, STONE_EDGE
	db 100, DRAGON_TAIL
	db 0 ; no more level-up moves

LugiaEvosAttacks:
	db 0 ; no more evolutions
	db 1, AEROBLAST
	db 11, SAFEGUARD
	db 22, HURRICANE
	db 33, RECOVER
	db 44, HYDRO_PUMP
	db 55, RAIN_DANCE
	db 66, SWIFT
	db 77, WHIRLWIND
	db 88, ANCIENTPOWER
	db 99, FUTURE_SIGHT
	db 0 ; no more level-up moves

HoOhEvosAttacks:
	db 0 ; no more evolutions
	db 1, SACRED_FIRE
	db 11, SAFEGUARD
	db 22, HURRICANE
	db 33, RECOVER
	db 44, FIRE_BLAST
	db 55, SUNNY_DAY
	db 66, SWIFT
	db 77, WHIRLWIND
	db 88, ANCIENTPOWER
	db 99, FUTURE_SIGHT
	db 0 ; no more level-up moves

CelebiEvosAttacks:
	db 0 ; no more evolutions
	db 1, LEECH_SEED
	db 1, PSYWAVE
	db 1, RECOVER
	db 1, HEAL_BELL
	db 10, SAFEGUARD
	db 20, ANCIENTPOWER
	db 30, FUTURE_SIGHT
	db 40, BATON_PASS
	db 50, PERISH_SONG
	db 0 ; no more level-up moves
