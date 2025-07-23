#!/bin/bash
set -e

if [[ -z $1 || $1 = '-h' || $1 = '--help' ]]; then
    printf "a simple tool for copying plain text into a rich text field (e.g. a tumblr post)\n"
    printf "that sidesteps it trying to coerce your special characters as\n"
    printf "markdown formatting or whatever\n\n"
    printf "usage: \e[1mpaste-xdotool.sh\e[0m \e[4msleep amount\e[0m\n"
    printf "\e[4msleep amount\e[0m controls the number of seconds to wait before starting to type\n"
    printf "input is read from \e[1mstdin\e[0m.\n"
    exit 1
fi

sleep $1

# IFS is a bash environment variable that controls word splitting, -r means don't treat the backslash as an escape
# character, -N1 means read one single character before returning.
#
# https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#index-read
while IFS= read -rN1 char; do
    # that preceding single quote is a extension POSIX shell printf makes to C printf, telling it to treat a character
    # as its numeric value
    #
    # https://pubs.opengroup.org/onlinepubs/9699919799/utilities/printf.html
    numeric_value=$(printf '%d' "'$char")

    # additionally convert linefeeds to shift+enter to avoid auto-paragraphing by rich text input fields
    keyboard_key=$(xkbcli how-to-type "${numeric_value}" | awk '{if ($2 == "Linefeed") { print "shift+enter" } else { print $2 }; exit}')

    xdotool key "${keyboard_key}"
done

# golfed:
#
# sleep $1;while IFS= read -rN1 char;do xdotool key $(xkbcli how-to-type $(printf '%d' "'$char")|awk '{if($2=="Linefeed"){print "shift+enter"}else{print $2};exit}');done
