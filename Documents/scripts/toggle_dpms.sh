#!/bin/bash
set -euxo pipefail

dpms_enabled=$(xset q | sed -nE 's/[[:space:]]+DPMS is (Enabled|Disabled)/\1/p')

if [ "$dpms_enabled" = "Enabled" ]; then
    xset s off -dpms
else
    xset s on +dpms
fi
