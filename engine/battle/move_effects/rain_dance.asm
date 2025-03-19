BattleCommand_StartRain:
	ld a, [wBattleWeather]
	cp WEATHER_RAIN
	jp z, .alreadyrain
	ld a, WEATHER_RAIN
	ld [wBattleWeather], a
	ld a, 5
	ld [wWeatherCount], a
	call AnimateCurrentMove
	ld hl, DownpourText
	jp StdBattleTextbox

.alreadyrain
	call AnimateFailedMove
	jp PrintButItFailed
