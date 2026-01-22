#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Launch recaf-gui.

# REQUIRES: bash, coreutils, java, recaf

# ==== SHELL OPTIONS ====

set -euxo pipefail

# ==== CONSTANTS ====

declare -r recaf_jar_dir=~/.local/lib/recaf-gui

# ==== CODE ====

pushd "${recaf_jar_dir}"

readarray -t recaf_jars < <(ls -1 .)

[[ "${#recaf_jars[@]}" -eq 1 ]] # assert there's only one

path_to_recaf="$(realpath "${recaf_jars[0]}")"

popd

nohup java -jar "${path_to_recaf}" &>/dev/null & # shut UP nohup
