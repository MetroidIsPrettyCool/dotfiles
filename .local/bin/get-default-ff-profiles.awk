#!/bin/awk -f

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Given some number of firefox ~profiles.ini~ files as input, parses them to extract whichever ones are marked as
# "default" and prints them out as a list, separated by newlines -- suitable for use with ~readarray -t~.

# If there are no profiles marked as default, prints nothing and exits with failure status code.

# REQUIRES: awk (duh)

# ==== CODE ====

BEGIN {
    FS = "="

    looking_for["install"] = 0
    looking_for["default"] = 1

    current_looking_for = looking_for["install"]
    current_install = ""
}

current_looking_for == looking_for["install"] && $0 ~ /^\[Install[[:digit:]ABCDEF]{16}\]$/ {
    current_install = substr($0, 2, length($0) - 2) # trim the first and last character (the brackets)
    current_looking_for = looking_for["default"]
    next
}

current_looking_for == looking_for["default"] && $0 ~ /^\[.*]$/ {
    current_install = ""
    current_looking_for = looking_for["install"]
    next
}

current_looking_for == looking_for["default"] && $1 == "Default" && current_install {
    default_profile_names[current_install] = $2
    current_install = ""
    current_looking_for = looking_for["install"]
    next
}

END {
    if (length(default_profile_names) == 0) {
        exit 1
    }
    for (i in default_profile_names) {
        printf "%s\n", default_profile_names[i]
    }
}
