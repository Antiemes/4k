:start
del intro-fast.exe
del intro-slow.exe
del intro-debug.exe
git pull
call build-fast.bat
call intro-fast.exe
pause
goto start
