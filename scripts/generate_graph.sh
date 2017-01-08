#!/usr/bin/env bash

prefix=$1

if [ -z "$prefix" ]; then
    prefix=dump_final
fi

echo "digraph {"

lsids()
{
    for file in log/$prefix*.log; do
        id=${file%%.log}
        id=${id##log/$prefix}
        echo $id
    done
}

for id in `lsids | sort -n`; do
    file=log/$prefix$id.log
    cat "$file" | while read peer; do
        if [[ "$peer" =~ ^edge[[:space:]]127.0.0.1:30([0-9]+) ]] ; then
            peer=${BASH_REMATCH[1]##0}
            echo "    $id -> $peer;"
        fi
    done
done

echo "}"
