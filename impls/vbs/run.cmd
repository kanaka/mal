@setlocal & @if not defined STEP set STEP=stepA_mal
@cscript -nologo "%~dp0\%STEP%.vbs" %*