MACRO mon_prob
; percent, index
	db \1, \2 * 2
ENDM

GrassMonProbTable:
	table_width 2
	mon_prob 15,  0 ; 15% chance
	mon_prob 29,  1 ; 14% chance
	mon_prob 43,  2 ; 14% chance
	mon_prob 58,  3 ; 15% chance
	mon_prob 72,  4 ; 14% chance
	mon_prob 86,  5 ; 14% chance
	mon_prob 100, 6 ; 14% chance
	assert_table_length NUM_GRASSMON

WaterMonProbTable:
	table_width 2
	mon_prob 34,  0 ; 34% chance
	mon_prob 67,  1 ; 33% chance
	mon_prob 100, 2 ; 33% chance
	assert_table_length NUM_WATERMON
