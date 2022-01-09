:: Code by OldLiu
:: using batch to achieve this program is a big challenge, but I still done it.
:: I hope you like it, lol.

@echo off
setlocal disabledelayedexpansion
for /f "delims==" %%a in ('set') do set "%%a="

:main
	set input=
	set /p "input=user> "
	if defined input (
		rem first replace double quotation mark.
		set "input=%input:"=This_is_a_double_quotation_mark,lol%"
		rem Batch can't deal with "!" when delayed expansion is enabled, so replace it to a special string.
		call set "input=%%input:!=This_is_a_Exclamation_Mark,lol%%"
		setlocal ENABLEDELAYEDEXPANSION
		%improve speed start% (
			rem Batch has some proble in "^" processing, so replace it.
			set "input=!input:^=This_is_a_caret,lol!"
			rem replace %.
			set input_formated=
			rem set input
			:replacement_loop
			if defined input (
				if "!input:~,1!" == "%%" (
					set "input_formated=!input_formated!This_is_a_percent_symbol,lol"
				) else (
					set "input_formated=!input_formated!!input:~,1!"
				)
				set "input=!input:~1!"
				goto replacement_loop
			)
			rem set input
			call :rep "!input_formated!"
			endlocal
		) %improve speed end%
	)
goto :main


%improve speed start% (
	:READ
		setlocal
			rem re means return, which bring return value.
			set "re=%~1"
		for /f "delims=" %%a in ("!re!") do (
			endlocal
			set "re=%%~a"
		)
	goto :eof

	:EVAL
		setlocal
			set "re=%~1"
		for /f "delims=" %%a in ("!re!") do (
			endlocal
			set "re=%%~a"
		)
	goto :eof

	:PRINT
		setlocal
			set "output=%~1"
			rem replace all speical symbol back.
			set output_buffer=
			:output_loop
			if "!output:~,30!" == "This_is_a_Exclamation_Mark,lol" (
				set "output_buffer=!output_buffer!^!"
				set "output=!output:~30!"
				goto output_loop
			) else if "!output:~,19!" == "This_is_a_caret,lol" (
				set "output_buffer=!output_buffer!^^"
				set "output=!output:~19!"
				goto output_loop
			) else if "!output:~,35!" == "This_is_a_double_quotation_mark,lol" (
				set "output_buffer=!output_buffer!^""
				set "output=!output:~35!"
				goto output_loop
			) else if "!output:~,1!" == "=" (
				set "output_buffer=!output_buffer!="
				set "output=!output:~1!"
				goto output_loop
			) else if "!output:~,1!" == " " (
				set "output_buffer=!output_buffer! "
				set "output=!output:~1!"
				goto output_loop
			) else if "!output:~,28!" == "This_is_a_percent_symbol,lol" (
				set "output_buffer=!output_buffer!%%"
				set "output=!output:~28!"
				goto output_loop
			) else if defined output (
				set "output_buffer=!output_buffer!!output:~,1!"
				set "output=!output:~1!"
				goto output_loop
			)
			echo.!output_buffer!
			set "re=%~1"
		for /f "delims=" %%a in ("!re!") do (
			endlocal
			set "re=%%~a"
		)
	goto :eof

	:rep
		setlocal
			call :READ "%~1"
			call :EVAL "!re!"
			call :PRINT "!re!"
		endlocal
	goto :eof
) %improve speed end%