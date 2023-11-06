@echo off

mkdir out
fpc src\sadv.pp -O4 -XX -Xs -FE"out/" -Fu"../src/" -Fu"../inc/" -Fu"inc/"

if not exist "C:\Program Files\sadv" mkdir "C:\Program Files\sadv"
mv out\sadv.exe "C:\Program Files\sadv\sadv.exe"

pause
