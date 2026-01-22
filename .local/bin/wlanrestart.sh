#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Take the wireless interface back down and try again to bring it up.

# KLUDGE for my stupid WiFi card not coming on properly after boot sometimes.

# TODO: Make this parse data from netctl to figure out what /should/ be up dynamically.

# REQUIRES: bash, ip, netctl

# ==== SHELL OPTIONS ====

set -euxo pipefail

# ==== CONSTANTS ====

declare -r default_profile_name='trogdor'
declare -r interface='wlan0'

# ==== CODE ====

profile_name="${1}"
if [[ -z "${1}" ]]; then
    profile_name="${default_profile_name}"
fi

sudo ip link set "${interface}" down
sudo netctl restart "${profile_name}"
