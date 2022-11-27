#!/bin/sh
T=`mktemp`
tail -c +125 "$0"|lzma -d|g++ -xc++ -O3 -pthread - -o /dev/fd/1>$T
chmod +x $T
(sleep 3;rm $T)&exec $T
