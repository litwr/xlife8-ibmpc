curdrv
if errorlevel 1 set D=B
if errorlevel 2 set D=C
if errorlevel 3 set D=D
if errorlevel 4 set D=E
if errorlevel 5 set D=F
if errorlevel 6 set D=G
if errorlevel 7 set D=H
if errorlevel 8 set D=I
if errorlevel 9 set D=J
if errorlevel 10 set D=K
if errorlevel 11 set D=L
if errorlevel 12 set D=M
if errorlevel 13 set D=N
if errorlevel 14 set D=O
if errorlevel 15 set D=P
if errorlevel 16 set D=Q
if errorlevel 17 set D=R
if errorlevel 18 set D=S
if errorlevel 19 set D=T
if errorlevel 20 set D=U
if errorlevel 21 set D=V
if errorlevel 22 set D=W
if errorlevel 23 set D=X
if errorlevel 24 set D=Y
if errorlevel 25 set D=Z
subst a: .
a:
call autoexec.bat
cls
%D%:
subst a: /d
rem pause
