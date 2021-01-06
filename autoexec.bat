echo off
:start
cls
echo 1. Xlife-8
echo 2. Manpage
echo 3. Notepad +4
echo 4. Default colors
echo 5. Exit to DOS
waitkey
if errorlevel 5 goto endp
if errorlevel 4 goto dcolors
if errorlevel 3 goto np4
if errorlevel 2 goto man
if errorlevel 1 xlife
goto start
:man
manpage
goto start
:np4
np4
goto start
:dcolors
del colors.cfg
goto start
:endp
