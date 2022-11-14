@echo off

echo \> Building...
fpc src\sadv.pas -O4 -FE"out/"

echo \> Creating Install Direcotry
mkdir "C:\Program Files\sadv"

echo \> Copying sadv.exe to Install Directory
mv out\sadv.exe "C:\Program Files\sadv\sadv.exe"

pause