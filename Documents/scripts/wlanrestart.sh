#!/bin/bash -x
set -e

profile_name="${1}"
if [[ -z "${1}" ]]; then
    profile_name='trogdor'
fi

sudo ip link set wlan0 down
sudo netctl restart "${profile_name}"
