#!/bin/bash
set -euxo pipefail

# mouse_name='pointer:Logitech MX Ergo Multi-Device Trackball '
mouse_name='pointer:Logitech MX Ergo'
mouse_props=$(xinput list-props "${mouse_name}" | tail --lines=+2) # +2 means all but the first line (1)

# note that xinput list-props will prepend a tab to all output lines, so $1 will be empty
#
# yes i know about awk -F, i prefer to take the verbose route on purpose when writing scripts because i know that in a
# week i'll have no clue what any given single-letter flag means
natural_scrolling_enabled=$(awk 'BEGIN { FS = "\t" }
$2 == "libinput Natural Scrolling Enabled (284):" {print $3; exit}'\
                                <<< "${mouse_props}")

# toggle!
xinput set-prop "${mouse_name}" 'libinput Natural Scrolling Enabled' $(( ($natural_scrolling_enabled + 1) % 2 ))

# golfed:
#
# x='pointer:Logitech MX Ergo';xinput set-prop "$x" 284 $((($(xinput list-props "$x" | grep '(284):' | tail -c 2)+1)%2))
