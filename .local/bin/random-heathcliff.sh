#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Doesn't really work anymore, 'cause Gocomics are all evil greedy bastards and forbade archive access to nonpaying
# members. (You can get around the paywall with proper use of console breakpoints, of course.)

# REQUIRES: bash, coreutils, xdg-utils

# ==== SHELL OPTIONS ====

set -euxo pipefail

# ==== CONSTANTS ====

# 1009843200 is 2002-01-01 @ 00:00:00 UTC
declare -r date_of_first_heathcliff_gocomics=1009843200

# ==== CODE ====

now=$(date +%s)

random_date=$(shuf -i "${date_of_first_heathcliff_gocomics}"-"${now}" -n 1)

#e.g. https://www.gocomics.com/heathcliff/2006/10/30
xdg-open "$(date -u --date='@'"${random_date}" '+https://www.gocomics.com/heathcliff/%Y/%m/%d')"
