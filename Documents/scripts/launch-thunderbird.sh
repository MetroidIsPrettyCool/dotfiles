#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Open Thunderbird in a new process, then update the color scheme with pywalfox.

# REQUIRES: bash, coreutils, thunderbird, procps-ng, pywalfox, wal

# ==== SHELL OPTIONS ====

set -euxo pipefail

# ==== CODE ====

thunderbird &

# wait for it to start
until [[ "$(ps --no-headers -C 'thunderbird' | wc --lines)" -ge 1 ]]; do
    sleep 0.1
done

sleep 1

# reset color scheme/theme
pywalfox update
