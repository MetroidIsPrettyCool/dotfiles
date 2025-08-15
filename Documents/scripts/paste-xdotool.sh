#!/bin/bash
set -euo pipefail

help_requested=''
sleep_delay='0'
shift_enter=''

# parse arguments
while [[ $# != 0 ]]; do
    case "${1}" in
        -s|--sleep-delay)
            sleep_delay="${2}"
            shift;;
        -h|--help)
            help_requested='t';;
        -r|--shift-enter)
            shift_enter='t';;
        *)
            printf "unknown argument: ${1}\n"
            exit 1;;
    esac
    shift
done


if [[ $help_requested = t ]]; then
    printf "Usage: \e[1mpaste-xdotool.sh\e[0m [\e[4mOPTIONS...\e[0m]\n"
    printf "\n"
    printf "A simple tool for copying plain text into places that don't support \"literal\" pasting,\n"
    printf "For example: the rich text field in the Tumblr post editor, which assumes unformatted\n"
    printf "text is markdown, or \e[3mMinecraft\e[0m signs before 18w45a/1.14.\n"
    printf "\n"
    printf "Those are what \e[3mI\e[0m use it for.\n"
    printf "\n"
    printf "Input is read from \e[4mstdin\e[0m.\n"
    printf "\n"
    printf "Arguments:\n"
    printf "\e[1m-h\e[0m, \e[1m--help\e[0m\n"
    printf "       Display this help message and exit\n"
    printf "\e[1m-s\e[0m, \e[1m--sleep-delay\e[0m\n"
    printf "       Set number of seconds to sleep before starting to type\n"
    printf "\e[1m-r\e[0m, \e[1m--shift-enter\e[0m\n"
    printf "       Hold the 'Shift' key while typing 'Enter'. useful for rich text fields\n"
    exit 1
fi

sleep "${sleep_delay}"

# i /think/ that's all the modifier keys. eventually i'll actually check.
xdotool keyup Shift_L Shift_R Caps_Lock Shift_Lock Control_L Control_R Alt_L Alt_R Meta_L Meta_R Num_Lock Super_L Super_R Hyper_L Hyper_R Mode_switch ISO_Level3_Shift ISO_Level5_Shift

# IFS is a bash environment variable that controls word splitting, -r means don't treat the backslash as an escape
# character, -N1 means read one single character before returning.
#
# https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-read
while IFS= read -rN1 char; do
    echo "${char}" >>~/test.txt

    # that preceding single quote down there is a extension POSIX shell printf makes to C printf, telling it to treat a
    # character as its numeric value
    #
    # https://pubs.opengroup.org/onlinepubs/9699919799/utilities/printf.html
    numeric_value=$(printf '%d' "'$char")

    # X keysyms shouldn't have spaces in their names, if i did my research right
    key_name="$(xkbcli how-to-type "${numeric_value}" \
                    | sed --quiet --regexp-extended \
                          '1s/^keysym: ([^[:space:]]*) \(0x[0123456789abcdefABCDEF]+\)$/\1/p')"

    # use the enter key, like we're a human being...
    if [[ $key_name = 'Linefeed' ]]; then
        key_name='enter'
        if [[ $shift_enter = t ]]; then
            key_name="shift+$key_name"
        fi
    fi

    # and type it!
    xdotool key "${key_name}"
done
