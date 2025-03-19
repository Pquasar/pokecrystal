object_const_def
const CHERRYGROVEGYMSPEECHHOUSE_POKEFAN_M
const CHERRYGROVEGYMSPEECHHOUSE_BUG_CATCHER

CherrygroveGymSpeechHouse_MapScripts:
def_scene_scripts

	def_callbacks



StarterTutorScript:
	faceplayer
	opentext
	writetext AskTeachAMoveText
	yesorno
	iffalse .Refused
	writetext NewBarkTownStarterTutorWhichMoveShouldITeachText
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
  ifequal 8, .ch15

.ch1:
  setval HEAL_BELL
  sjump .continuelearnmove

.ch2:
  setval LOW_KICK
  sjump .continuelearnmove

.ch3:
  setval IRON_TAIL
  sjump .continuelearnmove

.ch4:
  setval GIGA_DRAIN
  sjump .continuelearnmove

.ch5:
  setval SYNTHESIS
  sjump .continuelearnmove

.ch6:
  setval SIGNAL_BEAM
  sjump .continuelearnmove

.ch7:
  setval IRON_DEFENSE
  sjump .continuelearnmove

.ch8:
  setval FIRE_PUNCH
  sjump .continuelearnmove

.ch9:
  setval IRON_HEAD
  sjump .continuelearnmove

.ch10:
  setval AQUA_TAIL
  sjump .continuelearnmove

.ch11:
  setval PAIN_SPLIT
  sjump .continuelearnmove

.ch12:
  setval THUNDERPUNCH
  sjump .continuelearnmove

.ch13:
  setval ICY_WIND
  sjump .continuelearnmove

.ch14:
  setval ZEN_HEADBUTT
	sjump .continuelearnmove

.ch15:
	setval STEALTH_ROCK

.continuelearnmove:
  writetext NewBarkTownStarterTutorMoveText
  special MoveTutor
  ifequal FALSE, .TeachMove
  sjump .Incompatible

.Refused:
	writetext NewBarkTownStarterTutorAwwButTheyreAmazingText
	waitbutton
	closetext
	end

.Incompatible:
	closetext
	end

.TeachMove:
	writetext NewBarkTownStarterTutorIfYouUnderstandYouveMadeItText
	promptbutton
	writetext NewBarkTownStarterTutorFarewellKidText
	waitbutton
	closetext
	end

.MoveMenuHeader:
	db MENU_BACKUP_TILES ; flags
	menu_coords 0, 0, 15, 17
	dw .MenuData
	db 1 ; default option

.MenuData:
	db STATICMENU_CURSOR ; flags
	db 8; items
	db "HEAL BELL@"
	db "LOW KICK@"
	db "IRON TAIL@"
	db "GIGA DRAIN@"
  db "SYNTHESIS@"
  db "SIGNAL BEAM@"
  db "IRON DEFENSE@"
	db "NEXT@"

.MoveMenuHeader2:
	db MENU_BACKUP_TILES ; flags
	menu_coords 0, 0, 15, 17
	dw .MenuData2
	db 1 ; default option

.MenuData2:
	db STATICMENU_CURSOR ; flags
	db 8; items
	db "FIRE PUNCH@"
	db "IRON HEAD@"
	db "AQUA TAIL@"
	db "PAIN SPLIT@"
  db "THUNDERPUNCH@"
  db "ICY WIND@"
  db "ZEN HEADBUTT@"
	db "STEALTH ROCK@"

CherrygroveGymSpeechHousePokefanMScript:
jumptextfaceplayer CherrygroveGymSpeechHousePokefanMText

CherrygroveGymSpeechHouseBugCatcherScript:
jumptextfaceplayer CherrygroveGymSpeechHouseBugCatcherText

CherrygroveGymSpeechHouseBookshelf:
jumpstd PictureBookshelfScript

AskTeachAMoveText:
	text "I can teach your"
	line "#MON tutor"

	para "moves if you'd"
	line "like."

	para "Should I teach a"
	line "new move?"
	done


NewBarkTownStarterTutorAwwButTheyreAmazingText:
	text "Come back if"
	line "you want to"

	para "teach your"
	line "#MON a new"
	cont "move!"
	done

NewBarkTownStarterTutorWhichMoveShouldITeachText:
	text "Great! You won't"
	line "regret it!"

	para "Which move should"
	line "I teach?"
	done


NewBarkTownStarterTutorIfYouUnderstandYouveMadeItText:
	text "All done!"
  done

NewBarkTownStarterTutorFarewellKidText:
	text "Farewell and"
	line "good luck on"
	cont "your journey!"
	done

NewBarkTownStarterTutorMoveText:
	text_start
	done

CherrygroveGymSpeechHousePokefanMText:
text "You're trying to"
line "see how good you"

para "are as a #MON"
line "trainer?"

para "You better visit"
line "the #MON GYMS"

para "all over JOHTO and"
line "collect BADGES."
done

CherrygroveGymSpeechHouseBugCatcherText:
text "When I get older,"
line "I'm going to be a"
cont "GYM LEADER!"

para "I make my #MON"
line "battle with my"

para "friend's to make"
line "them tougher!"
done

CherrygroveGymSpeechHouse_MapEvents:
db 0, 0 ; filler

def_warp_events
warp_event  2,  7, CHERRYGROVE_CITY, 3
warp_event  3,  7, CHERRYGROVE_CITY, 3

def_coord_events

def_bg_events
bg_event  0,  1, BGEVENT_READ, CherrygroveGymSpeechHouseBookshelf
bg_event  1,  1, BGEVENT_READ, CherrygroveGymSpeechHouseBookshelf

def_object_events
object_event  2,  3, SPRITE_POKEFAN_M, SPRITEMOVEDATA_STANDING_DOWN, 0, 0, -1, -1, 0, OBJECTTYPE_SCRIPT, 0, CherrygroveGymSpeechHousePokefanMScript, -1
object_event  5,  5, SPRITE_BUG_CATCHER, SPRITEMOVEDATA_WALK_LEFT_RIGHT, 1, 0, -1, -1, PAL_NPC_RED, OBJECTTYPE_SCRIPT, 0, CherrygroveGymSpeechHouseBugCatcherScript, -1
object_event  1,  2, SPRITE_TEACHER, SPRITEMOVEDATA_STANDING_DOWN, 1, 0, -1, -1, PAL_NPC_BLUE, OBJECTTYPE_SCRIPT, 0, StarterTutorScript, -1
