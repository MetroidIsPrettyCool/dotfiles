#!/bin/bash
set -uxo pipefail

# terminate already running bar instances
killall --quiet polybar

set -e

# wait until the polybar processes have been shut down, and also until pulse is running
until [[ "$(ps --no-headers -C 'polybar' | wc --lines)" -eq 0 ]] && pulseaudio --check; do
    sleep 0.1
done
sleep 2.0

active_monitors_count=$(xrandr --listactivemonitors \
                            | sed --quiet --regexp-extended '1s/Monitors: ([[:digit:]]+)/\1/p')

# launch polybars for each monitor, using default config location ~/.config/polybar/config
if [[ active_monitors_count -ge 1 ]]; then
    polybar primary &
fi
if [[ active_monitors_count -ge 2 ]]; then
    polybar secondary &
fi
