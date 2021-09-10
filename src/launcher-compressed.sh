#!/bin/sh
T=`mktemp`
tail -c +112 "$0"|lzma -d|g++ -xc++ - -o /dev/fd/1>$T
chmod +x $T
(sleep 3;rm $T)&exec $T
