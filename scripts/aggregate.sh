#!/usr/bin/env bash

scenario=$1
prefix=$2

if [ -z "$scenario" ]; then
    echo "Usage: ./scripts/aggregate.sh <scenario_id> [log_prefix]"
    exit 1
fi

if [ -z "$prefix" ]; then
    prefix=dump_final
fi

source ./scripts/config/scenario$scenario.sh

lsids()
{
    for file in log/$prefix*.log; do
        id=${file%%.log}
        id=${id##log/$prefix}
        echo $id
    done
}

echo "node id,group id,group0,group1,group2"

for id in `lsids | sort -n`; do
    let "n01=$n0+$n1"
    if [[ $id -lt $n0 ]]; then
        group=0
    elif [[ $id -lt $n01 ]]; then
        group=1
    else
        group=2
    fi

    file=log/$prefix$id.log
    cnt1=0
    cnt2=0
    cnt3=0
    for peer in `cut -d ' ' -f1 $file`; do
        if [[ "$peer" =~ ^127.0.0.1:30([0-9]+) ]] ; then
            peer=${BASH_REMATCH[1]##0}
            if [[ $peer -lt $n0 ]]; then
                let 'cnt1++'
            elif [[ $peer -lt $n01 ]]; then
                let 'cnt2++'
            else
                let 'cnt3++'
            fi
        fi
    done
    echo "$id,$group,$cnt1,$cnt2,$cnt3"
done
