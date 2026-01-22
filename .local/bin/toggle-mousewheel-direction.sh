#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Toggle the scroll direction of my mouse.

# REQUIRES: awk, bash, coreutils, xinput

# ==== SHELL OPTIONS ====

set -euxo pipefail

# ==== CONSTANTS ====

# declare -r mouse_name='pointer:Logitech MX Ergo Multi-Device Trackball '
declare -r mouse_name='pointer:Logitech MX Ergo'

# ==== CODE ====

natural_scrolling_enabled=$(awk -F '\t' '$2 ~ /libinput Natural Scrolling Enabled/ {print $3; exit}' \
                                <(xinput list-props "${mouse_name}"))

# toggle!
xinput set-prop "${mouse_name}" 'libinput Natural Scrolling Enabled' $(( ! natural_scrolling_enabled ))
