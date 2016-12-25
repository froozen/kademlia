#!/usr/bin/env bash

source ./common.sh

prefix=$1

lsids()
{
    for file in log/$prefix*.log; do
        id=${file%%.log}
        id=${id##log/$prefix}
        echo $id
    done
}

echo "node id,group id,group1,group2"

for id in `lsids | sort -n`; do
    if [[ $id -lt $n1 ]]; then
        group=1
    else
        group=2
    fi

    file=log/$prefix$id.log
    cnt1=0
    cnt2=0
    for peer in `cut -d ' ' -f1 $file`; do
        if [[ "$peer" =~ ^127.0.0.1:30([0-9]+) ]] ; then
            peer=${BASH_REMATCH[1]##0}
            if [[ $peer -lt $n1 ]]; then
                let 'cnt1++'
            else
                let 'cnt2++'
            fi
        fi
    done
    echo "$id,$group,$cnt1,$cnt2"
done
