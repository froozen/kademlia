#!/usr/bin/env bash

export n0=10
export n1=80
export peer1=0
export n2=0
export peer2=0
export k=5
export routingSharingN=0
export t=60

let t2=$t*2; export t2

# Available commands:
#  group <group_number> Launch all nodes from <group_number>
#  sleep <seconds>      Sleep for <seconds> seconds
export scenario=$(cat <<EOF
group 0
sleep $t2
group 1
sleep $t2
sleep 20
EOF
)

# Script for nodes from group 0
export group0=$(cat <<EOF
sleep $t
dump dump_initial
sleep $t2
dump dump_final
sleep $t
EOF
)

# Script for nodes from group 1
export group1=$(cat <<EOF
sleep $t
dump dump_final
sleep $t
EOF
)

# Script for nodes from group 2
export group2=
