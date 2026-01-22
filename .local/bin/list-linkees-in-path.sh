#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Automatically go through $PATH and find executables that dynamically link to a given shared object.

# Sloppy, only made to help me figure out some otherwise sort of boring package cleanup decisions. libidn11, for one.

# REQUIRES: bash, coreutils, findutils, ldd

# ==== SHELL OPTIONS ====

set -euo pipefail

# ==== CODE ====

if [[ $# -ne 1 ]]; then
    printf "usage: %s [LIBSOMETHING.so]\n" "${0}"
    exit
fi

while read -r -d ':' p; do
    while IFS= read -r -d '' binary; do
        [[ -x "${binary}" ]] && ldd "${binary}" 2>/dev/null | grep -Fq "${1}" \
            && printf "%s\n" "${binary}"
    done < <(find "${p}" -maxdepth 1 -executable -type f -print0 2>/dev/null)
done <<< "${PATH}"

exit 0
