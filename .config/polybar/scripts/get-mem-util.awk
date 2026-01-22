#!/bin/awk -f

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Parse RAM usage from ~/proc/meminfo~ for the interesting bits.

# ==== CODE ====

$1 ~ /MemTotal/ {
    total_kb = $2
}

$1 ~ /MemAvailable/ {
    available_kb = $2
}

$1 ~ /MemFree/ {
    free_kb = $2
}

$1 ~ /SwapTotal/ {
    swap_total_kb = $2
}

$1 ~ /SwapFree/ {
    swap_free_kb = $2
}

END {
    # ~meminfo~ says it reports in units of kB (1000 bytes), but AFAICT these are kiB (1024 bytes).
    not_available_gib = (total_kb - available_kb) / 1024^2
    not_free_gib = (total_kb - free_kb) / 1024^2
    total_gib = total_kb / 1024^2

    swap_not_free_gib = (swap_total_kb - swap_free_kb) / 1024^2
    swap_total_gib = swap_total_kb / 1024^2

    printf "%4.1f/%4.1f/%4.1f", not_available_gib, not_free_gib, total_gib
    if (report_swap) {
        printf " (%4.1f/%4.1f)", swap_not_free_gib, swap_total_gib
    }

    printf " GiB\n"
}
