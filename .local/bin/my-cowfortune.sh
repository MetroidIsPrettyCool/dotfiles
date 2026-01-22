#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Cowfortune, basically, but we select a cowfile at random and afterwards print its name.

# TODO: more cowfiles!

# REQUIRES: bash, coreutils, cowsay, fortune

# ==== SHELL OPTIONS ====

set -euo pipefail

# ==== CONSTANTS ====

declare -ra cowdirs=('/usr/share/cowsay/cows/')

# ==== CODE ====

declare -a cowfiles=()

for dir in "${cowdirs[@]}"; do
    readarray -t -O"${#cowfiles[@]}" cowfiles < <(ls -1 "${dir}")
done

cowfile="${cowfiles[RANDOM % ${#cowfiles[@]}]}"

fortune -a | cowsay -n -f "${cowfile}"

basename "${cowfile}" .cow
