#!/usr/bin/env bash

source ./common.sh

mkdir -p log
rm -f log/*

for i in `seq 1 $n1`; do
    let "idx=$i - 1"
    echo "Launching node $idx"
    stack exec discovery $idx 0 >log/node$idx.log &
    sleep 0.5s
done

echo "Sleep $t"
sleep $t

for i in `seq 1 $n2`; do
    let "idx=$i + $n1 - 1"
    echo "Launching node $idx"
    stack exec discovery $idx 0 >log/node$idx.log &
    sleep 0.5s
done

echo "Sleep $t"
sleep $t
