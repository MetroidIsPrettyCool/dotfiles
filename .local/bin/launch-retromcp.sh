#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Start the retromcp CLI.

# REQUIRES: bash, coreutils, java, retromcp

# ==== SHELL OPTIONS ====

set -euxo pipefail

# ==== CONSTANTS ====

declare -r java8_bin_java=/usr/lib/jvm/java-8-openjdk/bin/java

declare -r retromcp_jar_dir=~/.local/lib/retromcp

# ==== CODE ====

pushd "${retromcp_jar_dir}"

readarray -t retromcp_jars < <(ls -1 .)

[[ "${#retromcp_jars[@]}" -eq 1 ]] # assert there's only one

path_to_retromcp="$(realpath "${retromcp_jars[0]}")"

popd

"${java8_bin_java}" -jar "${path_to_retromcp}"
