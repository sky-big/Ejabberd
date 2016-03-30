@echo off
echo Stopping ejabberd...
cd /D "C:\Program Files\ejabberd-16.02\bin"
bash ejabberdctl stop
if errorlevel 126 goto error
bash ejabberdctl stopped
if errorlevel 1 goto error
rundll32.exe url.dll,FileProtocolHandler "C:\Program Files\ejabberd-16.02\doc\stop.html"
goto end
:error
rundll32.exe url.dll,FileProtocolHandler "C:\Program Files\ejabberd-16.02\doc\error.html"
:end
