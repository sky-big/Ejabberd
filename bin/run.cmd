ECHO off

echo Starting Run Ejabberd ......

set MAKEFILE_FILE_CMD=%~dp0

cd /d %MAKEFILE_FILE_CMD%

bash run

echo Start Ejabberd Sucess ......
