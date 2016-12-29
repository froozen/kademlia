#!/usr/bin/env bash

scenario=$1

if [ -z "$scenario" ]; then
    scenario=0
fi

source ./scripts/config/scenario$scenario.sh

mkdir -p log
rm -f log/*

launch_group ()
{
    echo "Executing group $1"
    case $1 in
        0) start=0
           let end=$n0-1
           peer=0
           script=$group0
           ;;
        1) start=$n0
           let end=$n0+$n1-1
           peer=$peer1
           script=$group1
           ;;
        2) let start=$n0+$n1
           let end=$n0+$n1+$n2-1
           peer=$peer2
           script=$group2
           ;;
        *) echo "Unsupported group $1";
           exit 1
           ;;
    esac

    for i in `seq $start $end`; do
        echo "$script" | stack exec discovery $k $routingSharingN $i $peer $n0 $n1 $n2 &>log/stdout$i.log &
        sleep 0.25s
    done
}

echo "Started testing with settings:"
echo "N0=$n0"
echo "N1=$n1"
echo "N2=$n2"
echo "k=$k"
echo "routingSharingN=$routingSharingN"
echo "T=$t"
echo "peer1=$peer1"
echo "peer2=$peer2"

echo "$scenario" | while read line; do
    if [[ $line =~ ^group[[:space:]]([0-9]*) ]]; then
        echo "Executing group ${BASH_REMATCH[1]}"
        launch_group ${BASH_REMATCH[1]}
    elif [[ $line =~ ^sleep[[:space:]]([0-9]+) ]]; then
        echo "Sleep ${BASH_REMATCH[1]}"
        sleep ${BASH_REMATCH[1]}
    fi
done
