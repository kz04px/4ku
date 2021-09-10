#!/bin/sh
T=`mktemp`
tail -c +104 "$0"|g++ -xc++ - -o /dev/fd/1>$T
chmod +x $T
(sleep 3;rm $T)&exec $T
