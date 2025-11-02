#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Open Firefox in a new process, wait for the window to appear, move it to the specified i3 workspace, open a list of
# URLs in new tabs.

# REQUIRES: bash, coreutils, firefox, i3, jq

# ==== SHELL OPTIONS ====

set -euxo pipefail

# ==== CONSTANTS ====

declare -r destination_workspace='1'

declare -r window_class_regexp='^firefox$|^navigator$'

declare -ra open_these_in_new_tabs=('https://my.nintendo.com/')

# ==== CODE ====

firefox &

# wait for the window before trying to move it
subscribe_result="$(i3-msg -t subscribe '[ "window" ]')"
until [[ $(jq '(.change == "new")
                and (.container.window_properties.class | test("'"${window_class_regexp}"'" ; "i"))' \
           <<< "${subscribe_result}") \
         = true ]]
do
    subscribe_result="$(i3-msg -t subscribe '[ "window" ]')"
done

i3-msg --quiet '[class='"$(jq '.container.window_properties.class' <<< "${subscribe_result}")"'] focus'
i3-msg --quiet 'move window to workspace '"${destination_workspace}"

sleep 0.2

for url in "${open_these_in_new_tabs[@]}"; do
    firefox -new-tab "${url}"
done
