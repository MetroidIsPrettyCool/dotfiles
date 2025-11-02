#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Using data from `nvidia-smi`, print current GPU utilization percentage.

# REQUIRES: bash, coreutils, nvidia-smi

# ==== SHELL OPTIONS ====

set -euo pipefail

# ==== CODE ====

if ! util=$(nvidia-smi --query-gpu=utilization.gpu --format=csv,noheader,nounits); then
    printf "UNAVAILABLE"
    exit 255
fi

printf '%3d%%' "${util}"
