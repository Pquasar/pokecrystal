	object_const_def
	const CHERRYGROVEEVOLUTIONSPEECHHOUSE_LASS
	const CHERRYGROVEEVOLUTIONSPEECHHOUSE_YOUNGSTER

CherrygroveEvolutionSpeechHouse_MapScripts:
	def_scene_scripts

	def_callbacks



SecondTutorScript:
	faceplayer
	opentext
	writetext CherrygroveAskTeachAMoveText
	yesorno
	iffalse .Refused
	writetext CherrygroveStarterTutorWhichMoveShouldITeachText
  waitbutton
  closetext
  opentext
	loadmenu .MoveMenuHeader
	verticalmenu
	closewindow
	ifequal 1, .ch1
	ifequal 2, .ch2
	ifequal 3, .ch3
	ifequal 4, .ch4
  ifequal 5, .ch5
  ifequal 6, .ch6
  ifequal 7, .ch7
  ifequal 8, .page2
	sjump .Incompatible

.page2:
  closetext
  opentext
  loadmenu .MoveMenuHeader2
  verticalmenu
  closewindow
  ifequal 1, .ch8
  ifequal 2, .ch9
  ifequal 3, .ch10
  ifequal 4, .ch11
  ifequal 5, .ch12
  ifequal 6, .ch13
  ifequal 7, .ch14
  sjump .Incompatible

.ch1:
  setval SEED_BOMB
  sjump .continuelearnmove

.ch2:
  setval DRILL_RUN
  sjump .continuelearnmove

.ch3:
  setval ICE_PUNCH
  sjump .continuelearnmove

.ch4:
  setval LIQUIDATION
  sjump .continuelearnmove

.ch5:
  setval OUTRAGE
  sjump .continuelearnmove

.ch6:
  setval THROAT_CHOP
  sjump .continuelearnmove

.ch7:
  setval EARTH_POWER
  sjump .continuelearnmove

.ch8:
  setval GUNK_SHOT
  sjump .continuelearnmove

.ch9:
  setval DUAL_CHOP
  sjump .continuelearnmove

.ch10:
  setval DRAIN_PUNCH
  sjump .continuelearnmove

.ch11:
  setval HEAT_WAVE
  sjump .continuelearnmove

.ch12:
  setval HYPER_VOICE
  sjump .continuelearnmove

.ch13:
  setval KNOCK_OFF
  sjump .continuelearnmove

.ch14:
  setval DRAGON_PULSE

.continuelearnmove:
  writetext cherrygroveTownStarterTutorMoveText
  special MoveTutor
  ifequal FALSE, .TeachMove
  sjump .Incompatible

.Refused:
	writetext cherrygroveTownStarterTutorAwwButTheyreAmazingText
	waitbutton
	closetext
	end

.Incompatible:
	closetext
	end

.TeachMove:
	writetext cherrygroveTownStarterTutorIfYouUnderstandYouveMadeItText
	promptbutton
	waitbutton
	closetext

.MoveMenuHeader:
	db MENU_BACKUP_TILES ; flags
	menu_coords 0, 0, 15, 17
	dw .MenuData
	db 1 ; default option

.MenuData:
	db STATICMENU_CURSOR ; flags
	db 8; items
	db "SEED BOMB@"
	db "DRILL RUN@"
	db "ICE PUNCH@"
	db "LIQUIDATION@"
  db "OUTRAGE@"
  db "THROAT CHOP@"
  db "EARTH POWER@"
	db "NEXT@"

.MoveMenuHeader2:
	db MENU_BACKUP_TILES ; flags
	menu_coords 0, 0, 15, 17
	dw .MenuData2
	db 1 ; default option

.MenuData2:
	db STATICMENU_CURSOR ; flags
	db 8; items
	db "GUNK SHOT@"
	db "DUAL CHOP@"
	db "DRAIN PUNCH@"
	db "HEAT WAVE@"
  db "HYPER VOICE@"
  db "KNOCK OFF@"
  db "DRAGON PULSE@"
	db "CANCEL@"

CherrygroveAskTeachAMoveText:
	text "I can teach your"
	line "#MON tutor"

	para "moves if you'd"
	line "like."

	para "Should I teach a"
	line "new move?"
	done


cherrygroveTownStarterTutorAwwButTheyreAmazingText:
	text "Come back if"
	line "you want to"

	para "teach your"
	line "#MON a new"
	cont "move!"
	done

CherrygroveStarterTutorWhichMoveShouldITeachText:
	text "Great! You won't"
	line "regret it!"

	para "Which move should"
	line "I teach?"
	done


cherrygroveTownStarterTutorIfYouUnderstandYouveMadeItText:
	text "All done!"
  done

cherrygroveTownStarterTutorMoveText:
	text_start
	done

CherrygroveEvolutionSpeechHouseYoungsterScript:
	opentext
	writetext CherrygroveEvolutionSpeechHouseYoungsterText
	waitbutton
	closetext
	end

CherrygroveEvolutionSpeechHouseLassScript:
	opentext
	writetext CherrygroveEvolutionSpeechHouseLassText
	waitbutton
	closetext
	end

CherrygroveEvolutionSpeechHouseBookshelf:
	jumpstd MagazineBookshelfScript

CherrygroveEvolutionSpeechHouseYoungsterText:
	text "#MON gain expe-"
	line "rience in battle"

	para "and change their"
	line "form."
	done

CherrygroveEvolutionSpeechHouseLassText:
	text "#MON change?"

	para "I would be shocked"
	line "if one did that!"
	done

CherrygroveEvolutionSpeechHouse_MapEvents:
	db 0, 0 ; filler

	def_warp_events
	warp_event  2,  7, CHERRYGROVE_CITY, 5
	warp_event  3,  7, CHERRYGROVE_CITY, 5

	def_coord_events

	def_bg_events
	bg_event  0,  1, BGEVENT_READ, CherrygroveEvolutionSpeechHouseBookshelf
	bg_event  1,  1, BGEVENT_READ, CherrygroveEvolutionSpeechHouseBookshelf

	def_object_events
	object_event  3,  5, SPRITE_LASS, SPRITEMOVEDATA_STANDING_LEFT, 0, 0, -1, -1, PAL_NPC_GREEN, OBJECTTYPE_SCRIPT, 0, CherrygroveEvolutionSpeechHouseLassScript, -1
	object_event  2,  5, SPRITE_YOUNGSTER, SPRITEMOVEDATA_STANDING_RIGHT, 0, 0, -1, -1, PAL_NPC_RED, OBJECTTYPE_SCRIPT, 0, CherrygroveEvolutionSpeechHouseYoungsterScript, -1
	object_event  2,  2, SPRITE_SAGE, SPRITEMOVEDATA_STANDING_DOWN, 0, 0, -1, -1, PAL_NPC_RED, OBJECTTYPE_SCRIPT, 0, SecondTutorScript, -1
