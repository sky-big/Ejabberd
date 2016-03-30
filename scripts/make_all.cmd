@echo off

echo Starting Make Ejabberd .....

del ..\ebin\*.beam

set MAKEFILE_FILE_CMD=%~dp0

cd /d %MAKEFILE_FILE_CMD%

bash make

echo Make Ejabberd Success !!!

pause

