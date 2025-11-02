#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Kill polybar (if running), wait until pulseaudio is running, then start a polybar for each detected monitor.

# REQUIRES: bash, coreutils, polybar, procps-ng, psmisc, pulseaudio, sed, xorg-xrandr

# ==== SHELL OPTIONS ====

set -euxo pipefail

# ==== CODE ====

# Terminate already running bar instances, allowed to fail.

set +e
killall --quiet polybar
set -e

# Wait until the polybar processes have been shut down, and also until pulse is running, to do anything else.
until [[ "$(ps --no-headers -C 'polybar' | wc --lines)" -eq 0 ]] && pulseaudio --check; do
    sleep 0.1
done
sleep 2.0                       # I don't remember why this is so long. INREQ: is this needed?

active_monitors_count=$(xrandr --listactivemonitors \
                            | sed --quiet --regexp-extended '1s/Monitors: ([[:digit:]]+)/\1/p')

# Launch polybars for each monitor, using default config location ~/.config/polybar/config.
if [[ active_monitors_count -ge 1 ]]; then
    polybar primary &
fi
if [[ active_monitors_count -ge 2 ]]; then
    polybar secondary &
fi
