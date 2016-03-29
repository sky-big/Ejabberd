rem @echo off
cd /D "C:\Program Files\ejabberd-16.02\bin"

rem Create runtime directories in APPDATA
if not exist "%APPDATA%\ejabberd\conf" mkdir "%APPDATA%\ejabberd\conf"
if not exist "%APPDATA%\ejabberd\logs" mkdir "%APPDATA%\ejabberd\logs"
if not exist "%APPDATA%\ejabberd\database" mkdir "%APPDATA%\ejabberd\database"
if not exist "%APPDATA%\ejabberd\www" mkdir "%APPDATA%\ejabberd\www"

rem Move runtime files in APPDATA
for /d %%I in (..) do set PARENT=%%~fI
@for %%I in (..\conf\*.*) do @if not exist "%APPDATA%\ejabberd\conf\%%~nxI" (
    echo copy %%~nxI
    copy "%%I" "%APPDATA%\ejabberd\conf\%%~nxI"
    if /i "%%~nxI" EQU "ejabberd.yml" (
        sed -i "/conf/ s!%PARENT:\=\\%!%APPDATA:\=\\%\\ejabberd!g" "%APPDATA%\ejabberd\conf\ejabberd.yml"
        sed -i "/C:/ s!/!\\!g" "%APPDATA%\ejabberd\conf\ejabberd.yml"
        sed -i "s!\\!\\\\!g;s!$!\r!g" "%APPDATA%\ejabberd\conf\ejabberd.yml"
    )
)
copy ..\conf\inetrc "%APPDATA%\ejabberd\conf"
rmdir ..\conf ..\database /s /q

rem Fix for erlang to work
sed -i "s!\\!/!g" erl.ini

rem Register ejabberd service
rem erlsrv add ejabberd -sname ejabberd-srv@xxw-PC -w "C:\Program Files\ejabberd-16.02\bin" -ar "-s win_service" -st "win_service:stop()." -onfail restart

rem Start ejabberd
bash ejabberdctl start
if errorlevel 126 goto error
bash ejabberdctl started
if errorlevel 1 goto error

rem Register admin user
bash ejabberdctl register "%1" "%2" "%3"

rem Stop ejabberd
bash ejabberdctl stop
bash ejabberdctl stopped

rem Symlink logs directory
cd ..
rmdir logs /s /q
mklink /d logs "%APPDATA%\ejabberd\logs"

exit 0

:error
echo Cannot start ejabberd
exit 1
