#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Execute a given command on every orphaned tilde file in a given directory, recursively. (That being: any file who's
# path or name matches the regex /(.*)~/ when \1 does not exist in the same directory.)

# Not that useful compared to what Emacs can do by itself, and not that well-implemented. I just wanted the experience
# with case statements.

# REQUIRES: bash, coreutils, findutils, sed

# ==== SHELL OPTIONS ====

set -euo pipefail

# ==== CODE ====

display_help_instead='false'
origin="${PWD}"
manage_command='echo'

unknown_arg=''

# == PARSE ARGUMENTS ==

prev_arg=''
for arg in "$@"; do
    case "$prev_arg" in
        "-o" | "--origin")
            origin="${arg}"
            ;;
        "-c" | "--command")
            manage_command="${arg}"
            ;;
        *)
            case "$arg" in
                "-o" | "--origin")
                ;;
                "-c" | "--command")
                ;;
                "-h" | "--help")
                    display_help_instead="true"
                    ;;
                *)
                    unknown_arg="$arg"
                    break
                    ;;
            esac
            ;;

    esac
    prev_arg="${arg}"
done

if [[ ! -z "${unknown_arg}" ]]; then
    printf "error: unknown argument: %s\n" "${unknown_arg}"
    exit 1
fi

if [[ "${display_help_instead}" = "true" ]]; then
    printf "usage: manage-orphan-tilde-files [flags]

flags:
            -h |            --help :: display this message

      -o PATH  |    --origin PATH  :: change the search root directory (default is current directory)

    -c COMMAND | --command COMMAND :: change the command to execute per-orphan (default is echo)\n"

    exit
fi

# == FIND ==

readarray -t tilde_files < <(find "${origin}" -name '*~' -exec printf '%s\n' {} \;)

# == EXECUTE ==

for tilde_file in "${tilde_files[@]}"; do
    parent_name=$(sed --quiet 's/\(.*\)~/\1/p' <<< "${tilde_file}")

    # if it doesn't have a parent
    if [[ ! -e "${parent_name}" ]]; then
        "${manage_command}" "${tilde_file}"
    fi
done
