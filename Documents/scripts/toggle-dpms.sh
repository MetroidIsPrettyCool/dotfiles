#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Toggle "Display Power Management Signaling".

# REQUIRES: bash, sed, xorg-xset

# ==== SHELL OPTIONS ====

set -euxo pipefail

# ==== CODE ====

dpms_enabled=$(xset q | sed -nE 's/[[:space:]]+DPMS is (Enabled|Disabled)/\1/p')

if [[ "$dpms_enabled" = "Enabled" ]]; then
    xset s off -dpms
else
    xset s on +dpms
fi

# Golfed version, tested only on my machine, fairly fragile:

# xset s o`xset q|grep -q En &&echo ff\ - ||echo n\ +`dpms
