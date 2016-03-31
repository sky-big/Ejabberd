@echo off
cd /D "C:\Program Files\ejabberd-16.02\bin"
rem stop ejabberd
bash ejabberdctl stop
bash ejabberdctl stopped
rem unregister service
erlsrv remove ejabberd
rem force epmd stop
epmd -kill
cd ..
rmdir logs
