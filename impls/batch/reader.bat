:: Code by OldLiu
:: using batch to achieve this program is a big challenge, but I still done it.
:: I hope you like it, lol.


::Start
	Set "_TMP_Arguments_=%*"
	If "!_TMP_Arguments_:~,1!" Equ ":" (
		Set "_TMP_Arguments_=!_TMP_Arguments_:~1!"
	)
	Call :!_TMP_Arguments_!
	Set _TMP_Arguments_=
Goto :Eof

:read_str
	setlocal
	for %%a in ("!re!") do (
		endlocal
		set "re=%%~a"
	)
goto :eof


:read_form code
	setlocal
		set "code=%~1"
        call :_delete_space code
        if "!code:~,1!" == "(" (
            call :read_list "!code:~1!"
        ) else (
            call :read_atom "!code:~1!"
        )
	for %%a in ("!re!") do (
		endlocal
		set "re=%%~a"
	)
goto :eof

:read_list code
	setlocal
		set "code=%~1"
        call :_delete_space code
	for %%a in ("!re!") do (
		endlocal
		set "re=%%~a"
	)
goto :eof

:read_atom code
	setlocal
		set "code=%~1"
        call :_delete_space code
	for %%a in ("!re!") do (
		endlocal
		set "re=%%~a"
	)
goto :eof

:_delete_space var
    if "!%1:~,1!" == " " (
        set "%1=!%1:~1!"
        goto :_delete_space
    )
goto :eof