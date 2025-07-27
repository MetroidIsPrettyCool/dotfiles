#!/bin/bash
set -euxo pipefail

thunderbird &

# wait for it to start
processes=$(ps -f -u $EUID) # -f is "do full-format listing" and -u is "userlist"
until [[ $(grep --count 'thunderbird' <<< "${processes}") -ge 1 ]]; do
    sleep 0.1
    processes=$(ps -f -u $EUID)
done

sleep 1

# reset color scheme/theme
pywalfox update
