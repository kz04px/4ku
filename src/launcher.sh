#!/bin/sh
T=`mktemp`
tail -c +117 "$0"|g++ -xc++ -O3 -pthread - -o /dev/fd/1>$T
chmod +x $T
(sleep 3;rm $T)&exec $T
