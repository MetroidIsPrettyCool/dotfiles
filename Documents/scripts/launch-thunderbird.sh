#!/bin/bash
set -euxo pipefail

thunderbird &

# wait for it to start
until [[ "$(ps --no-headers -C 'thunderbird' | wc --lines)" -ge 1 ]]; do
    sleep 0.1
done

sleep 1

# reset color scheme/theme
pywalfox update
