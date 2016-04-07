@echo off

set RUN_FILE_FILE_CMD=%~dp0

cd /d %RUN_FILE_FILE_CMD%

bash ejabberdctl "%1" "%2" "%3" "%4" "%5" "%6" "%7" "%8" "%9"
