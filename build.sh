#!/usr/bin/bash

mkdir -p out
rm out/*

if [[ $1 == debug ]]; then
  fpc src/sadv.pp -Fu"inc/" -FE"out/" -g -dDEBUG
else
  fpc src/sadv.pp -Fu"inc/" -FE"out/" -O4 -Xs -XX
fi
