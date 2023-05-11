@echo off

mkdir out
fpc src\sadv.pas -O4 -XX -Xs -FE"out/"

if not exist "C:\Program Files\sadv" mkdir "C:\Program Files\sadv"
mv out\sadv.exe "C:\Program Files\sadv\sadv.exe"

pause
