@echo off
echo Starting ejabberd...
if not exist "%USERPROFILE%" (
echo Home directory is unreachable. It is needed for .erlang.cookie.
goto error
)
cd /D "C:\Program Files\ejabberd-16.02\bin"
bash ejabberdctl start
if errorlevel 126 goto error
bash ejabberdctl started
if errorlevel 1 goto error
rundll32.exe url.dll,FileProtocolHandler "C:\Program Files\ejabberd-16.02\doc\start.html"
goto end
:error
rundll32.exe url.dll,FileProtocolHandler "C:\Program Files\ejabberd-16.02\doc\error.html"
:end
