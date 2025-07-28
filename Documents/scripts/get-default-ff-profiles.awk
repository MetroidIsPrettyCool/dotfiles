#!/bin/awk -f

BEGIN {
    looking_for = "install"
    FS = "="
    current_install = "none"
}

$0 ~ /^\[Install[[:digit:]ABCDEF]{16}\]$/ && looking_for == "install" {
    current_install = substr($0, 2, length($0) - 2) # trim the first and last character (the brackets)
    looking_for = "default"
    next
}

$0 ~ /^\[.*]$/ && looking_for == "default" {
    current_install = "none"
    looking_for = "install"
    next
}

$1 == "Default" && looking_for == "default" && current_install != "none" {
    default_profile_names[current_install] = $2
    current_install = "none"
    looking_for = "install"
    next
}

END {
    if (length(default_profile_names) == 0) {
        exit 1
    }
    is_first_entry = "false"
    for (i in default_profile_names) {
        if (is_first_entry == "false") {
            printf "%s", default_profile_names[i]
            is_first_entry = "true"
        } else {
            is_first_entry = "true"
            printf " %s", default_profile_names[i]
        }
    }
    printf "\n"
}
