#!/bin/sh

prefix="$1"
for i in 0 1 2 3 4 5 6 7 8 9 ; do
    echo "model ${prefix}$i"
    ./run.sh skeleton ./problems/F/FA${prefix}$i_tgt.mdl ./res/FA${prefix}$i.nbt
done
