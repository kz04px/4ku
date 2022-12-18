#!/bin/sh
T=`mktemp`
tail -c +139 "$0"|lzma -d|g++ -xc++ -O3 -march=native -pthread - -o /dev/fd/1>$T
chmod +x $T
(sleep 3;rm $T)&exec $T
