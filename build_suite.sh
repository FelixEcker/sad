#!/usr/bin/sh
# Builds the entire sadsuite
mkdir -p saduite_bin
rm saduite_bin/** -rf

cd sadv
sh build.sh
cd ../sadhtml
sh build.sh

cp sadv/out/sadv saduite_bin/
cp sadhtml/out/sadhtml sadsuite_bin/
