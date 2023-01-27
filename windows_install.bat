@echo off

mkdir out
fpc src\sadv.pas -O4 -FE"out/"

mkdir "C:\Program Files\sadv"
mv out\sadv.exe "C:\Program Files\sadv\sadv.exe"

pause
