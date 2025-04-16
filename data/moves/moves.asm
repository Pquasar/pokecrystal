; Characteristics of each move.

MACRO move
	db \1 ; animation
	db \2 ; effect
	db \3 ; power
	db \4 | \5 ; type
	db \6 percent ; accuracy
	db \7 ; pp
	db \8 percent ; effect chance
	assert \7 <= 40, "PP must be 40 or less"
ENDM

Moves:
; entries correspond to move ids (see constants/move_constants.asm)
	table_width MOVE_LENGTH, Moves
	move POUND,        EFFECT_NORMAL_HIT,         40, NORMAL,        PHYSICAL, 100, 40,   0 ; can remove
	move GRASS_KNOT,   EFFECT_LOW_KICK,            1, GRASS,         SPECIAL,  100, 40,   0 ; ANIM NEED TO DO
	move DEFOG,        EFFECT_DEFOG,               0, NORMAL,        PHYSICAL, 100, 24,   0 ; ANIM NEED TO DO
	move SHELL_SMASH,  EFFECT_SHELL_SMASH,         0, NORMAL,        STATUS,   100, 24,   0 ; ANIM NEED TO DO
	move FOUL_PLAY,    EFFECT_NORMAL_HIT,         95, DARK,          PHYSICAL, 100, 24,   0 ; ANIM OK
	move PAY_DAY,      EFFECT_PAY_DAY,            40, NORMAL,        PHYSICAL, 100, 32,   0 ; can remove
	move FIRE_PUNCH,   EFFECT_BURN_HIT,           75, FIRE,          PHYSICAL, 100, 24,  10 
	move ICE_PUNCH,    EFFECT_FREEZE_HIT,         75, ICE,           PHYSICAL, 100, 24,  10
	move THUNDERPUNCH, EFFECT_PARALYZE_HIT,       75, ELECTRIC,      PHYSICAL, 100, 24,  10
	move LAVA_PLUME,   EFFECT_BURN_HIT,           80, FIRE,          SPECIAL,  100, 24,  30 ; ANIM DONE
	move SLUDGE_WAVE,  EFFECT_POISON_HIT,         95, POISON,        SPECIAL,  100, 16,  10 ; ANIM NEED TO DO
	move LEAF_STORM,   EFFECT_OVERHEAT,          130, GRASS,         SPECIAL,   90,  8,   0 ; ANIM OK
	move SUPERPOWER,   EFFECT_SUPERPOWER,        120, FIGHTING,      PHYSICAL, 100,  8,   0 ; ANIM OK
	move SWORDS_DANCE, EFFECT_ATTACK_UP_2,         0, NORMAL,        STATUS,   100, 32,   0
	move CUT,          EFFECT_NORMAL_HIT,         50, NORMAL,        PHYSICAL,  95, 40,   0
	move CLEAR_SMOG,   EFFECT_CLEAR_SMOG,         50, POISON,        SPECIAL,  100, 40,   0 ; ANIM OK
	move WING_ATTACK,  EFFECT_NORMAL_HIT,         60, FLYING,        PHYSICAL, 100, 40,   0 ; can remove
	move WHIRLWIND,    EFFECT_FORCE_SWITCH,        0, NORMAL,        STATUS,   100, 32,   0
	move FLY,          EFFECT_FLY,                90, FLYING,        PHYSICAL,  95, 24,   0
	move BIND,         EFFECT_TRAP_TARGET,        15, NORMAL,        PHYSICAL,  85, 32,   0
	move VOLT_SWITCH,  EFFECT_U_TURN,             70, ELECTRIC,      SPECIAL,  100, 32,   0 ; ANIM OK MAYBE
	move STEALTH_ROCK, EFFECT_STEALTH_ROCK,        0, ROCK,          PHYSICAL, 100, 16,   0 ; ANIM OK
	move KNOCK_OFF,    EFFECT_KNOCK_OFF,          65, DARK,          PHYSICAL, 100, 32, 100 ; ANIM DONE
	move WEATHER_BALL, EFFECT_WEATHER_BALL,       50, NORMAL,        SPECIAL,  100, 16,   0 
	move FAKE_OUT,     EFFECT_FAKE_OUT,           40, NORMAL,        PHYSICAL, 100, 16,   0 ; ANIM NEED TO DO
	move JUMP_KICK,    EFFECT_JUMP_KICK,         100, FIGHTING,      PHYSICAL,  95, 40,   0
	move NASTY_PLOT,   EFFECT_SP_ATK_UP_2,         0, DARK,          STATUS,   100, 24,   0 ; ANIM DONE
	move FREEZE_DRY,   EFFECT_FREEZE_HIT,         70, FREEZEDRY,     SPECIAL,  100, 32,  10 ; ANIM DONE
	move HEADBUTT,     EFFECT_FLINCH_HIT,         70, NORMAL,        PHYSICAL, 100, 24,  30
	move THROAT_CHOP,  EFFECT_NORMAL_HIT,         80, DARK,          PHYSICAL, 100, 24,   0 ; ANIM OK MAYBE
	move PLAY_ROUGH,   EFFECT_ATTACK_DOWN_HIT,    90, FAIRY,         PHYSICAL,  90, 16,   0 ; ANIM DONE
	move AQUA_JET,     EFFECT_PRIORITY_HIT,       40, WATER,         PHYSICAL, 100, 32,   0 ; ANIM DONE
	move TACKLE,       EFFECT_NORMAL_HIT,         40, NORMAL,        PHYSICAL,  95, 40,   0 ; can remove
	move BODY_SLAM,    EFFECT_PARALYZE_HIT,       85, NORMAL,        PHYSICAL, 100, 24,  30
	move WRAP,         EFFECT_TRAP_TARGET,        15, NORMAL,        PHYSICAL,  90, 32,   0
	move FLARE_BLITZ,  EFFECT_RECOIL_HIT,        120, FIRE,          PHYSICAL, 100, 24,   0 ; ANIM NEED TO DO
	move THRASH,       EFFECT_RAMPAGE,           120, NORMAL,        PHYSICAL, 100, 16,   0
	move DOUBLE_EDGE,  EFFECT_RECOIL_HIT,        120, NORMAL,        PHYSICAL, 100, 24,   0
	move ACID_SPRAY,   EFFECT_ACID_SPRAY,         40, POISON,        SPECIAL,   90, 16,  10 ; ANIM OK
	move HAMMER_ARM,   EFFECT_HAMMER_ARM,        100, FIGHTING,      PHYSICAL,  90, 16,   0 ; ANIM DONE
	move LEAF_BLADE,   EFFECT_NORMAL_HIT,         90, GRASS,         PHYSICAL, 100, 24,   0 ; ANIM DONE
	move PIN_MISSILE,  EFFECT_MULTI_HIT,          25, BUG,           PHYSICAL,  95, 32,   0 
	move DRAGON_PULSE, EFFECT_NORMAL_HIT,         85, DRAGON,        SPECIAL,  100, 16,   0 ; ANIM DONE
	move HONE_CLAWS,   EFFECT_HONE_CLAWS,          0, DARK,          STATUS,   100, 24,   0 ; ANIM DONE
	move HEX,          EFFECT_HEX,                65, GHOST,         SPECIAL,  100, 16,   0 ; ANIM DONE
	move ROAR,         EFFECT_FORCE_SWITCH,        0, NORMAL,        STATUS,   100, 32,   0 
	move COSMIC_POWER, EFFECT_STOCKPILE,           0, PSYCHIC_TYPE,  STATUS,   100, 32,   0 ; ANIM OK
	move DUAL_CHOP,    EFFECT_DOUBLE_HIT,         40, DRAGON,        PHYSICAL,  90, 24,   0 ; ANIM NEED TO DO
	move ICICLE_CRASH, EFFECT_FLINCH_HIT,         85, ICE,           PHYSICAL,  90, 16,  30 ; ANIM DONE
	move DISABLE,      EFFECT_DISABLE,             0, NORMAL,        STATUS,   100, 32,   0
	move POWER_WHIP,   EFFECT_NORMAL_HIT,        120, GRASS,         PHYSICAL,  85, 16,   0 ; ANIM NEED TO DO
	move DRAIN_PUNCH,  EFFECT_LEECH_HIT,          75, FIGHTING,      PHYSICAL, 100, 16,  10 ; ANIM DONE
	move FLAMETHROWER, EFFECT_BURN_HIT,           90, FIRE,          SPECIAL,  100, 24,  10
	move MIST,         EFFECT_MIST,                0, ICE,           STATUS,   100, 40,   0
	move AURA_SPHERE,  EFFECT_ALWAYS_HIT,         80, FIGHTING,      SPECIAL,  100, 32,   0 ; ANIM DONE
	move HYDRO_PUMP,   EFFECT_NORMAL_HIT,        110, WATER,         SPECIAL,   80,  8,   0
	move SURF,         EFFECT_NORMAL_HIT,         90, WATER,         SPECIAL,  100, 24,   0
	move ICE_BEAM,     EFFECT_FREEZE_HIT,         90, ICE,           SPECIAL,  100, 16,  10
	move BLIZZARD,     EFFECT_BLIZZARD,          110, ICE,           SPECIAL,   70,  8,  10
	move TOXIC_SPIKES, EFFECT_TOXIC_SPIKES,        0, POISON,        STATUS,   100, 32,   0
	move BUBBLEBEAM,   EFFECT_SPEED_DOWN_HIT,     65, WATER,         SPECIAL,  100, 32,  10 ; can remove
	move AURORA_BEAM,  EFFECT_ATTACK_DOWN_HIT,    65, ICE,           SPECIAL,  100, 32,  10 ; can remove
	move HYPER_BEAM,   EFFECT_HYPER_BEAM,        150, NORMAL,        SPECIAL,   90,  8,   0
	move DAZLINGGLEAM, EFFECT_NORMAL_HIT,         80, FAIRY,         SPECIAL,  100, 16,   0 ; ANIM NEED TO DO
	move DRILL_PECK,   EFFECT_NORMAL_HIT,         80, FLYING,        PHYSICAL, 100, 32,   0
	move DARK_PULSE,   EFFECT_FLINCH_HIT,         80, DARK,          SPECIAL,  100, 24,  20 ; ANIM DONE
	move LOW_KICK,     EFFECT_LOW_KICK,            1, FIGHTING,      PHYSICAL, 100, 32,  30
	move COUNTER,      EFFECT_COUNTER,             1, FIGHTING,      PHYSICAL, 100, 32,   0
	move SEISMIC_TOSS, EFFECT_LEVEL_DAMAGE,        1, FIGHTING,      PHYSICAL, 100, 32,   0
	move STRENGTH,     EFFECT_NORMAL_HIT,         80, NORMAL,        PHYSICAL, 100, 24,   0
	move WILD_CHARGE,  EFFECT_RECOIL_HIT,         90, ELECTRIC,      PHYSICAL, 100, 24,   0 ; ANIM DONE
	move FLASH_CANNON, EFFECT_SP_DEF_DOWN_HIT,    80, STEEL,         SPECIAL,  100, 16,  10 ; ANIM DONE
	move LEECH_SEED,   EFFECT_LEECH_SEED,          0, GRASS,         STATUS,    90, 16,   0
	move GROWTH,       EFFECT_GROWTH,              0, NORMAL,        STATUS,   100, 32,   0
	move ROCK_POLISH,  EFFECT_SPEED_UP_2,          0, ROCK,          STATUS,   100, 32,   0
	move SOLARBEAM,    EFFECT_SOLARBEAM,         120, GRASS,         SPECIAL,  100, 16,   0
	move POISONPOWDER, EFFECT_POISON,              0, POISON,        STATUS,    75, 40,   0
	move STUN_SPORE,   EFFECT_PARALYZE,            0, GRASS,         STATUS,    75, 40,   0
	move SLEEP_POWDER, EFFECT_SLEEP,               0, GRASS,         STATUS,    75, 24,   0
	move PETAL_DANCE,  EFFECT_RAMPAGE,           120, GRASS,         SPECIAL,  100, 16,   0
	move BULLDOZE,     EFFECT_SPEED_DOWN_HIT,     60, GROUND,        SPECIAL,  100, 32, 100 ; ANIM DONE
	move OVERHEAT,     EFFECT_OVERHEAT,          130, FIRE,          SPECIAL,   90,  8,   0 ; ANIM DONE
	move FIRE_SPIN,    EFFECT_TRAP_TARGET,        35, FIRE,          SPECIAL,   85, 24,   0
	move HAIL,         EFFECT_HAIL,                0, ICE,           STATUS,   100, 16,  10
	move THUNDERBOLT,  EFFECT_PARALYZE_HIT,       90, ELECTRIC,      SPECIAL,  100, 24,  10
	move THUNDER_WAVE, EFFECT_PARALYZE,            0, ELECTRIC,      STATUS,    90, 32,   0
	move THUNDER,      EFFECT_THUNDER,           110, ELECTRIC,      SPECIAL,   70, 16,  30
	move DRAGON_CLAW,  EFFECT_NORMAL_HIT,         80, DRAGON,        PHYSICAL, 100, 24,   0 ; ANIM DONE
	move EARTHQUAKE,   EFFECT_EARTHQUAKE,        100, GROUND,        PHYSICAL, 100, 16,   0
	move DRAGON_TAIL,  EFFECT_DRAGON_TAIL,        60, DRAGON,        PHYSICAL,  90, 16,   0
	move DIG,          EFFECT_FLY,                80, GROUND,        PHYSICAL, 100, 16,   0
	move TOXIC,        EFFECT_TOXIC,               0, POISON,        STATUS,    90, 16,   0
	move CROSS_POISON, EFFECT_POISON_HIT,         70, POISON,        PHYSICAL, 100, 32,  10 ; ANIM DONE
	move PSYCHIC_M,    EFFECT_SP_DEF_DOWN_HIT,    90, PSYCHIC_TYPE,  SPECIAL,  100, 16,  10
	move HYPNOSIS,     EFFECT_SLEEP,               0, PSYCHIC_TYPE,  STATUS,    60, 32,   0
	move IRON_HEAD,    EFFECT_FLINCH_HIT,         80, STEEL,         PHYSICAL, 100, 24,  30 ; ANIM DONE
	move AGILITY,      EFFECT_SPEED_UP_2,          0, PSYCHIC_TYPE,  STATUS,   100, 40,   0
	move QUICK_ATTACK, EFFECT_PRIORITY_HIT,       40, NORMAL,        PHYSICAL, 100, 40,   0
	move RAGE,         EFFECT_RAGE,               20, NORMAL,        PHYSICAL, 100, 32,   0 ; can remove
	move TELEPORT,     EFFECT_TELEPORT,            0, PSYCHIC_TYPE,  STATUS,   100, 32,   0
	move NIGHT_SHADE,  EFFECT_LEVEL_DAMAGE,        1, GHOST,         SPECIAL,  100, 24,   0
	move MIMIC,        EFFECT_MIMIC,               0, NORMAL,        STATUS,   100, 16,   0 ; can remove
	move SCREECH,      EFFECT_DEFENSE_DOWN_2,      0, NORMAL,        STATUS,    85, 40,   0
	move FOCUS_BLAST,  EFFECT_SP_DEF_DOWN_HIT,   120, FIGHTING,      SPECIAL,   70,  8,  10 ; ANIM DONE
	move RECOVER,      EFFECT_HEAL,                0, NORMAL,        STATUS,   100, 16,   0
	move SHADOW_CLAW,  EFFECT_NORMAL_HIT,         70, GHOST,         PHYSICAL, 100, 24,   0 ; ANIM DONE
	move MINIMIZE,     EFFECT_EVASION_UP,          0, NORMAL,        STATUS,   100, 32,   0 ; can remove
	move COIL,         EFFECT_COIL,                0, POISON,        STATUS,   100, 32,   0 ; ANIM OK
	move CONFUSE_RAY,  EFFECT_CONFUSE,             0, GHOST,         STATUS,   100, 16,   0
	move BRAVE_BIRD,   EFFECT_RECOIL_HIT,        120, FLYING,        PHYSICAL, 100, 24,   0 ; ANIM DONE
	move DEFENSE_CURL, EFFECT_DEFENSE_CURL,        0, NORMAL,        STATUS,   100, 40,   0 ; can remove
	move BARRIER,      EFFECT_DEFENSE_UP_2,        0, PSYCHIC_TYPE,  STATUS,   100, 32,   0
	move LIGHT_SCREEN, EFFECT_LIGHT_SCREEN,        0, PSYCHIC_TYPE,  STATUS,   100, 40,   0
	move HAZE,         EFFECT_RESET_STATS,         0, ICE,           STATUS,   100, 40,   0
	move REFLECT,      EFFECT_REFLECT,             0, PSYCHIC_TYPE,  STATUS,   100, 32,   0
	move FOCUS_ENERGY, EFFECT_FOCUS_ENERGY,        0, NORMAL,        STATUS,   100, 40,   0
	move BIDE,         EFFECT_BIDE,                0, NORMAL,        PHYSICAL, 100, 16,   0 ; can remove
	move METRONOME,    EFFECT_METRONOME,           0, NORMAL,        STATUS,   100, 16,   0
	move MIRROR_MOVE,  EFFECT_MIRROR_MOVE,         0, FLYING,        STATUS,   100, 32,   0 ; can remove
	move SELFDESTRUCT, EFFECT_SELFDESTRUCT,      200, NORMAL,        PHYSICAL, 100,  8,   0
	move GUNK_SHOT,    EFFECT_POISON_HIT,        120, POISON,        PHYSICAL,  80,  8,  30 ; ANIM DONE
	move POWER_GEM,    EFFECT_NORMAL_HIT,         80, ROCK,          SPECIAL,  100, 32,   0 ; ANIM DONE
	move SEED_BOMB,    EFFECT_NORMAL_HIT,         80, GRASS,         PHYSICAL, 100, 24,   0 ; ANIM DONE
	move SLUDGE,       EFFECT_POISON_HIT,         65, POISON,        SPECIAL,  100, 32,  30 ; can remove
	move AQUA_TAIL,    EFFECT_NORMAL_HIT,         90, WATER,         PHYSICAL,  90, 16,   0 ; ANIM DONE
	move FIRE_BLAST,   EFFECT_BURN_HIT,          110, FIRE,          SPECIAL,   85,  8,  10
	move WATERFALL,    EFFECT_FLINCH_HIT,         80, WATER,         PHYSICAL, 100, 24,  30
	move CLAMP,        EFFECT_TRAP_TARGET,        35, WATER,         PHYSICAL,  85, 24,   0
	move SWIFT,        EFFECT_ALWAYS_HIT,         60, NORMAL,        SPECIAL,  100, 32,   0 ; can remove
	move SKULL_BASH,   EFFECT_SKULL_BASH,        130, NORMAL,        PHYSICAL, 100, 16,   0
	move HEAT_WAVE,    EFFECT_BURN_HIT,           95, FIRE,          SPECIAL,   90, 16,  10 ; ANIM OK
	move HYPER_VOICE,  EFFECT_NORMAL_HIT,         90, NORMAL,        SPECIAL,  100, 16,   0 ; ANIM DONE
	move AMNESIA,      EFFECT_SP_DEF_UP_2,         0, PSYCHIC_TYPE,  STATUS,   100, 32,   0
	move HURRICANE,    EFFECT_HURRICANE,         110, FLYING,        SPECIAL,   70, 16,  30 ; ANIM DONE
	move SOFTBOILED,   EFFECT_HEAL,                0, NORMAL,        STATUS,   100, 16,   0
	move HI_JUMP_KICK, EFFECT_JUMP_KICK,         130, FIGHTING,      PHYSICAL,  90, 16,   0
	move GLARE,        EFFECT_PARALYZE,            0, NORMAL,        STATUS,   100, 40,   0
	move DREAM_EATER,  EFFECT_DREAM_EATER,       100, PSYCHIC_TYPE,  SPECIAL,  100, 24,   0
	move POISON_GAS,   EFFECT_POISON,              0, POISON,        STATUS,    90, 40,   0
	move EXTRASENSORY, EFFECT_FLINCH_HIT,         80, PSYCHIC_TYPE,  SPECIAL,  100, 32,  10 ; ANIM DONE
	move LEECH_LIFE,   EFFECT_LEECH_HIT,          80, BUG,           PHYSICAL, 100, 16,   0
	move LOVELY_KISS,  EFFECT_SLEEP,               0, NORMAL,        STATUS,    75, 16,   0
	move SKY_ATTACK,   EFFECT_SKY_ATTACK,        140, FLYING,        PHYSICAL,  90,  8,   0 ; can remove
	move TRANSFORM,    EFFECT_TRANSFORM,           0, NORMAL,        STATUS,   100, 16,   0
	move ZEN_HEADBUTT, EFFECT_FLINCH_HIT,         80, PSYCHIC_TYPE,  PHYSICAL,  90, 24,  20 ; ANIM DONE
	move DIZZY_PUNCH,  EFFECT_CONFUSE_HIT,        70, NORMAL,        PHYSICAL, 100, 16,  20 ; can remove
	move SPORE,        EFFECT_SLEEP,               0, GRASS,         STATUS,   100, 24,   0
	move FLASH,        EFFECT_ACCURACY_DOWN,       0, NORMAL,        STATUS,   100, 32,   0
	move PSYWAVE,      EFFECT_PSYWAVE,             1, PSYCHIC_TYPE,  SPECIAL,  100, 24,   0 ; can remove
	move MOONBLAST,    EFFECT_SP_ATK_DOWN_HIT,    95, FAIRY,         SPECIAL,  100, 24,  30 ; ANIM DONE
	move ACID_ARMOR,   EFFECT_DEFENSE_UP_2,        0, POISON,        STATUS,   100, 32,   0
	move CRABHAMMER,   EFFECT_NORMAL_HIT,        100, WATER,         PHYSICAL,  90, 16,   0
	move EXPLOSION,    EFFECT_SELFDESTRUCT,      250, NORMAL,        PHYSICAL, 100,  8,   0
	move ICE_SHARD,    EFFECT_PRIORITY_HIT,       40, ICE,           PHYSICAL, 100, 40,   0 ; ANIM DONE
	move BONEMERANG,   EFFECT_DOUBLE_HIT,         50, GROUND,        PHYSICAL,  90, 16,   0
	move REST,         EFFECT_HEAL,                0, PSYCHIC_TYPE,  STATUS,   100, 16,   0
	move ROCK_SLIDE,   EFFECT_FLINCH_HIT,         75, ROCK,          PHYSICAL,  90, 16,  30
	move HYPER_FANG,   EFFECT_FLINCH_HIT,         80, NORMAL,        PHYSICAL,  90, 24,  10 ; can remove
	move CALM_MIND,    EFFECT_CALM_MIND,           0, PSYCHIC_TYPE,  STATUS,   100, 32,   0 ; ANIM DONE
	move BULLET_PUNCH, EFFECT_PRIORITY_HIT,       40, STEEL,         PHYSICAL, 100, 40,   0 ; ANIM DONE
	move TRI_ATTACK,   EFFECT_TRI_ATTACK,         80, NORMAL,        SPECIAL,  100, 16,  20
	move STONE_EDGE,   EFFECT_NORMAL_HIT,        100, ROCK,          PHYSICAL,  80,  8,   0 ; ANIM DONE
	move SLASH,        EFFECT_NORMAL_HIT,         70, NORMAL,        PHYSICAL, 100, 32,   0
	move SUBSTITUTE,   EFFECT_SUBSTITUTE,          0, NORMAL,        STATUS,   100, 16,   0
	move STRUGGLE,     EFFECT_RECOIL_HIT,         50, NORMAL,        PHYSICAL, 100,  1,   0
	move SKETCH,       EFFECT_SKETCH,              0, NORMAL,        STATUS,   100,  1,   0
	move BULK_UP,      EFFECT_BULK_UP,             0, FIGHTING,      STATUS,   100, 32,   0 ; ANIM DONE
	move THIEF,        EFFECT_THIEF,              60, DARK,          PHYSICAL, 100, 40, 100 
	move SPIDER_WEB,   EFFECT_MEAN_LOOK,           0, BUG,           STATUS,   100, 16,   0
	move GYRO_BALL,    EFFECT_GYRO_BALL,           1, STEEL,         PHYSICAL, 100,  8,   0 ; ANIM NEED TO DO
	move NIGHTMARE,    EFFECT_NIGHTMARE,           0, GHOST,         STATUS,   100, 24,   0
	move CLOSE_COMBAT, EFFECT_CLOSE_COMBAT,      120, FIGHTING,      PHYSICAL, 100,  8,   0 ; ANIM DONE
	move SNORE,        EFFECT_SNORE,              50, NORMAL,        SPECIAL,  100, 24,  30 ; can remove
	move CURSE,        EFFECT_CURSE,               0, GHOST,         STATUS,   100, 16,   0
	move FLAIL,        EFFECT_REVERSAL,            1, NORMAL,        PHYSICAL, 100, 24,   0
	move SCALD,        EFFECT_BURN_HIT,           80, WATER,         SPECIAL,  100, 24,  30 ; ANIM DONE
	move AEROBLAST,    EFFECT_NORMAL_HIT,        100, FLYING,        SPECIAL,   95,  8,   0
	move STOCKPILE,    EFFECT_STOCKPILE,           0, NORMAL,        STATUS,   100, 32,   0 ; ANIM OK
	move REVERSAL,     EFFECT_REVERSAL,            1, FIGHTING,      PHYSICAL, 100, 24,   0
	move WILL_O_WISP,  EFFECT_BURN,                0, FIRE,          STATUS,    85, 24,   0 ; ANIM DONE
	move EARTH_POWER,  EFFECT_SP_DEF_DOWN_HIT,    90, GROUND,        SPECIAL,  100, 16,  10 ; ANIM NEED TO DO
	move PROTECT,      EFFECT_PROTECT,             0, NORMAL,        STATUS,   100, 16,   0
	move MACH_PUNCH,   EFFECT_PRIORITY_HIT,       40, FIGHTING,      PHYSICAL, 100, 40,   0
	move SCARY_FACE,   EFFECT_SPEED_DOWN_2,        0, NORMAL,        STATUS,    90, 16,   0
	move FAINT_ATTACK, EFFECT_ALWAYS_HIT,         60, DARK,          PHYSICAL, 100, 32,   0 ; can remove
	move SHADOW_SNEAK, EFFECT_PRIORITY_HIT,       40, GHOST,         PHYSICAL, 100, 40,   0 ; ANIM DONE
	move BELLY_DRUM,   EFFECT_BELLY_DRUM,          0, NORMAL,        STATUS,   100, 16,   0
	move SLUDGE_BOMB,  EFFECT_POISON_HIT,         90, POISON,        SPECIAL,  100, 16,  30
	move SUCKER_PUNCH, EFFECT_SUCKER_PUNCH,       70, DARK,          PHYSICAL, 100, 16,   0 ; ANIM DONE
	move INFESTATION,  EFFECT_TRAP_TARGET,        20, BUG,           SPECIAL,  100, 32,   0
	move SPIKES,       EFFECT_SPIKES,              0, GROUND,        STATUS,   100, 32,   0
	move ZAP_CANNON,   EFFECT_PARALYZE_HIT,      120, ELECTRIC,      SPECIAL,   50,  8, 100
	move ENERGY_BALL,  EFFECT_SP_DEF_DOWN_HIT,    90, GRASS,         SPECIAL,  100, 16,  10 ; ANIM DONE
	move DESTINY_BOND, EFFECT_DESTINY_BOND,        0, GHOST,         STATUS,   100,  8,   0
	move PERISH_SONG,  EFFECT_PERISH_SONG,         0, NORMAL,        STATUS,   100,  8,   0
	move ICY_WIND,     EFFECT_SPEED_DOWN_HIT,     55, ICE,           SPECIAL,   95, 24, 100
	move DETECT,       EFFECT_PROTECT,             0, FIGHTING,      STATUS,   100,  8,   0
	move BUG_BUZZ,     EFFECT_SP_DEF_DOWN_HIT,    90, BUG,           SPECIAL,  100, 16,  10 ; ANIM DONE
	move LIQUIDATION,  EFFECT_DEFENSE_DOWN_HIT,   85, WATER,         PHYSICAL, 100, 16,  20 ; ANIM NEED TO DO
	move OUTRAGE,      EFFECT_RAMPAGE,           120, DRAGON,        PHYSICAL, 100, 16,   0
	move SANDSTORM,    EFFECT_SANDSTORM,           0, ROCK,          STATUS,   100, 16,   0
	move GIGA_DRAIN,   EFFECT_LEECH_HIT,          75, GRASS,         SPECIAL,  100, 16,   0
	move ENDURE,       EFFECT_ENDURE,              0, NORMAL,        STATUS,   100, 16,   0
	move CHARM,        EFFECT_ATTACK_DOWN_2,       0, FAIRY,         STATUS,   100, 32,   0
	move ROLLOUT,      EFFECT_ROLLOUT,            30, ROCK,          PHYSICAL,  90, 32,   0
	move QUIVER_DANCE, EFFECT_QUIVER_DANCE,        0, BUG,           STATUS,   100, 32,   0 ; ANIM DONE
	move SWAGGER,      EFFECT_SWAGGER,             0, NORMAL,        STATUS,    85, 24, 100 ; can remove
	move MILK_DRINK,   EFFECT_HEAL,                0, NORMAL,        STATUS,   100, 16,   0
	move DRILL_RUN,    EFFECT_NORMAL_HIT,         80, GROUND,        PHYSICAL,  95, 16,   0 ; ANIM OK
	move FURY_CUTTER,  EFFECT_FURY_CUTTER,        40, BUG,           PHYSICAL,  95, 32,   0
	move STEEL_WING,   EFFECT_DEFENSE_UP_HIT,     70, STEEL,         PHYSICAL,  90, 40,  10
	move MEAN_LOOK,    EFFECT_MEAN_LOOK,           0, NORMAL,        STATUS,   100,  8,   0
	move ATTRACT,      EFFECT_ATTRACT,             0, NORMAL,        STATUS,   100, 24,   0
	move SLEEP_TALK,   EFFECT_SLEEP_TALK,          0, NORMAL,        STATUS,   100, 16,   0
	move HEAL_BELL,    EFFECT_HEAL_BELL,           0, NORMAL,        STATUS,   100,  8,   0
	move RETURN,       EFFECT_NORMAL_HIT,        102, NORMAL,        PHYSICAL, 100, 32,   0
	move PRESENT,      EFFECT_PRESENT,             1, NORMAL,        PHYSICAL,  90, 24,   0 ; can remove
	move IRON_DEFENSE, EFFECT_DEFENSE_UP_2,        0, STEEL,         STATUS,   100, 24,   0 ; ANIM DONE
	move SAFEGUARD,    EFFECT_SAFEGUARD,           0, NORMAL,        STATUS,   100, 40,   0
	move PAIN_SPLIT,   EFFECT_PAIN_SPLIT,          0, NORMAL,        STATUS,   100, 32,   0
	move SACRED_FIRE,  EFFECT_SACRED_FIRE,       100, FIRE,          PHYSICAL,  95,  8,  50
	move MAGNITUDE,    EFFECT_MAGNITUDE,           1, GROUND,        PHYSICAL, 100, 40,   0 ; can remove
	move BRICK_BREAK,  EFFECT_BRICK_BREAK,        75, FIGHTING,      PHYSICAL, 100, 24,   0 ; ANIM NEED TO DO 
	move MEGAHORN,     EFFECT_NORMAL_HIT,        120, BUG,           PHYSICAL,  85, 16,   0
	move ACROBATICS,   EFFECT_ACROBATICS,         55, FLYING,        PHYSICAL, 100, 24,   0 ; ANIM DONE
	move BATON_PASS,   EFFECT_BATON_PASS,          0, NORMAL,        STATUS,   100, 40,   0
	move ENCORE,       EFFECT_ENCORE,              0, NORMAL,        STATUS,   100,  8,   0
	move PURSUIT,      EFFECT_PURSUIT,            40, DARK,          PHYSICAL, 100, 32,   0
	move RAPID_SPIN,   EFFECT_RAPID_SPIN,         20, NORMAL,        PHYSICAL, 100, 40,   0
	move SWEET_SCENT,  EFFECT_EVASION_DOWN,        0, NORMAL,        STATUS,   100, 32,   0
	move IRON_TAIL,    EFFECT_DEFENSE_DOWN_HIT,  100, STEEL,         PHYSICAL,  75, 24,  30
	move ROOST,        EFFECT_HEAL,                0, FLYING,        STATUS,   100,  8,   0 ; ANIM NEED TO DO
	move U_TURN,       EFFECT_U_TURN,             70, BUG,           PHYSICAL, 100, 32,   0 ; ANIM NEED TO DO
	move MORNING_SUN,  EFFECT_MORNING_SUN,         0, NORMAL,        STATUS,   100,  8,   0
	move SYNTHESIS,    EFFECT_SYNTHESIS,           0, GRASS,         STATUS,   100,  8,   0
	move MOONLIGHT,    EFFECT_MOONLIGHT,           0, FAIRY,         STATUS,   100,  8,   0
	move HIDDEN_POWER, EFFECT_HIDDEN_POWER,       60, NORMAL,        SPECIAL,  100, 24,   0
	move CROSS_CHOP,   EFFECT_NORMAL_HIT,        100, FIGHTING,      PHYSICAL,  80,  8,   0
	move X_SCISSOR,    EFFECT_NORMAL_HIT,         80, BUG,           PHYSICAL, 100, 24,   0 ; ANIM DONE
	move RAIN_DANCE,   EFFECT_RAIN_DANCE,          0, WATER,         STATUS,    90,  8,   0
	move SUNNY_DAY,    EFFECT_SUNNY_DAY,           0, FIRE,          STATUS,    90,  8,   0
	move CRUNCH,       EFFECT_DEFENSE_DOWN_HIT,   80, DARK,          PHYSICAL, 100, 24,  20
	move MIRROR_COAT,  EFFECT_MIRROR_COAT,         1, PSYCHIC_TYPE,  SPECIAL,  100, 32,   0
	move DRAGON_DANCE, EFFECT_DRAGON_UP,           0, DRAGON,        STATUS,   100, 32,   0 ; ANIM DONE
	move EXTREMESPEED, EFFECT_PRIORITY_HIT,       80, NORMAL,        PHYSICAL, 100,  8,   0
	move ANCIENTPOWER, EFFECT_ALL_UP_HIT,         60, ROCK,          SPECIAL,  100,  8,  10
	move SHADOW_BALL,  EFFECT_SP_DEF_DOWN_HIT,    80, GHOST,         SPECIAL,  100, 24,  20
	move FUTURE_SIGHT, EFFECT_FUTURE_SIGHT,      120, PSYCHIC_TYPE,  SPECIAL,  100, 16,   0
	move ROCK_SMASH,   EFFECT_DEFENSE_DOWN_HIT,   40, FIGHTING,      PHYSICAL, 100, 24,  50
	move WHIRLPOOL,    EFFECT_TRAP_TARGET,        35, WATER,         SPECIAL,   85, 24,   0
	move POISON_JAB,   EFFECT_POISON_HIT,         80, POISON,        PHYSICAL, 100, 32,  30 ; ANIM DONE
	move SIGNAL_BEAM,  EFFECT_CONFUSE_HIT,        75, BUG,           SPECIAL,  100, 24,  10 ; ANIM DONE
	move NIGHT_SLASH,  EFFECT_NORMAL_HIT,         70, DARK,          PHYSICAL, 100, 24,   0 ; ANIM DONE
	move AIR_SLASH,    EFFECT_FLINCH_HIT,         75, FLYING,        SPECIAL,   95, 24,  30 ; ANIM DONE
	assert_table_length NUM_ATTACKS
