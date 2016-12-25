#!/usr/bin/env bash

source ./common.sh

mkdir -p log
rm -f log/*

for i in `seq 1 $n1`; do
    let "idx=$i - 1"
    echo "Launching node $idx"
    stack exec discovery $idx 0 0 &>log/stdout$idx.log &
    sleep 0.5s
done

echo "Sleep $t"
sleep $t

for i in `seq 1 $n2`; do
    let "idx=$i + $n1 - 1"
    let "peer=0"
    peer=0
    echo "Launching node $idx"
    stack exec discovery $idx $peer 1 &>log/stdout$idx.log &
done

echo "Sleep $t+$t+10"
sleep $t
sleep $t
sleep 10
