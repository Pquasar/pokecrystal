BattleCommand_StartSun:
	ld a, [wBattleWeather]
	cp WEATHER_SUN
	jp z, .alreadysun
	ld a, WEATHER_SUN
	ld [wBattleWeather], a
	ld a, 5
	ld [wWeatherCount], a
	call AnimateCurrentMove
	ld hl, SunGotBrightText
	jp StdBattleTextbox

.alreadysun
	call AnimateFailedMove
	jp PrintButItFailed
