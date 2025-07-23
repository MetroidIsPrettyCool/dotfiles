#!/bin/bash
set -e

# as in any file who's path/name matches the regex `/(.*)~/`, and `\1` does not exist in the same directory

display_help_instead='false'
origin="${PWD}"
manage_command='echo'

unknown_arg=''

# ==== PARSE ARGUMENTS ====
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
    return 1
fi

if [[ "${display_help_instead}" = "true" ]]; then
    printf "usage: manage-orphan-tilde-files [flags]

flags:
            -h |            --help :: display this message

      -o PATH  |    --origin PATH  :: change the search root directory (default is current directory)

    -c COMMAND | --command COMMAND :: change the command to execute per-orphan (default is echo)\n"

    return 1
fi

# ==== FIND ====

# change IFS to ensure spaces get treated properly by bash's array parser
#
# '🏑' is just some random character. I picked it 'cause the name started with 'field'.
#
# hopefully nobody is creating files with that in their names
saved_ifs="$IFS"
IFS=🏑

tilde_files=($(find "${origin}" -name '*~' -exec printf '%s🏑' {} \;))

# ==== EXECUTE ====
for tilde_file in "${tilde_files[@]}"; do
    parent_name=$(sed --quiet 's/\(.*\)~/\1/p' <<< "${tilde_file}")

    # if it doesn't have a parent
    if [[ ! -e "${parent_name}" ]]; then
        "${manage_command}" ${tilde_file}
    fi
done

# reset IFS
IFS="$saved_ifs"
