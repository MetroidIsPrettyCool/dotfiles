#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Using data from `nvidia-smi`, print the current GPU temperature. Returns with failure exit code if temp is higher than
# `$warning_temp`, so as to signal to polybar it should use warning colors.

# REQUIRES: bash, coreutils, nvidia-smi

# ==== SHELL OPTIONS ====

set -euo pipefail

# ==== CONSTANTS ====

declare -r warning_temp=80

# ==== CODE ====

if ! temp_c=$(nvidia-smi --query-gpu=temperature.gpu --format=csv,noheader,nounits); then
    printf "UNAVAILABLE"
    exit 255
fi

# temp_f=$(($temp_c * 9 / 5 + 32))
# echo $temp_c°C/$temp_f°F

printf "%s°C" "${temp_c}"

if [[ $temp_c -ge $warning_temp ]]; then
    exit 255
else
    exit 0
fi
