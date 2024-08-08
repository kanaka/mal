@echo off & setlocal
if not defined STEP set STEP=stepA_mal

set "SCRIPT=%~dp0\%STEP%.vbs"
cscript //nologo "%SCRIPT%" %*