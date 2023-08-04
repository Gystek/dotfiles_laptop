#!/bin/sh
OUT=$(mktemp -d)
CUR=$(pwd)

git clone git://git.suckless.org/st "$OUT"

cd "$OUT"
find "$CUR" -name '*.diff' -exec git apply {} \;
ln -s "$CUR/config.h" config.h
make
sudo make install
cd -
