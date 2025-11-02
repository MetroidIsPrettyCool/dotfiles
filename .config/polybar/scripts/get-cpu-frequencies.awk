#!/bin/awk -f

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Print the min, mean, max and standard distribution of CPU frequencies when given /proc/cpuinfo as input.

# ==== CODE ====

BEGIN {
    FS = "\t*:"
    count = 0
}

# AFAICT, `/proc/cpuinfo` reporting in megahertz is standardized
$1 == "cpu MHz" {
    # divide to turn MHz into Ghz
    freq_ghz = $2 / 1000

    frequencies[count] = freq_ghz
    sum += freq_ghz
    count += 1

    if (min_freq == "") {
        min_freq = max_freq = freq_ghz
    } else if (freq_ghz < min_freq) {
        min_freq = freq_ghz
    } else if (freq_ghz > max_freq) {
        max_freq = freq_ghz
    }
}

END {
    mean_freq = sum / count

    for (i in frequencies) {
        sd_sum += (frequencies[i] - mean_freq) ** 2
    }

    freq_sd = sqrt(sd_sum / count)

    printf "%.2f/%.2f/%.2f/%.2f\n", min_freq, mean_freq, max_freq, freq_sd
}
