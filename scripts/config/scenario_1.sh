#!/usr/bin/env bash

# Size of group 0
export n0=20
# Size of group 1
export n1=30
# Initial peer for group 1
export peer1=0
# Size of group 2
export n2=0
# Initial peer for group 2
export peer2=0
# parameter of kademlia
export k=12
# parameter of kademlia
export routingSharingN=0
# parameter of kademlia
export pingTime=10
# Common prefix for keys for nodes from groups 1 and 2
export prefixLen=17
# Time in seconds to wait until routing tables stabilize
export t=60
# Number of edges to dump for each bucket
export bctEdges=1

let t2=$t*2; export t2

# Available commands:
#  group <group_number> Launch all nodes from <group_number>
#  sleep <seconds>      Sleep for <seconds> seconds
export scenario=$(cat <<EOF
group 0
group 1
sleep $t2
sleep 20
EOF
)

# Script for nodes from group 0
export group0=$(cat <<EOF
sleep $t
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
