@echo off
wsl.exe --distribution Debian --exec /usr/local/bin/rupam %*
exit /b %errorlevel%
