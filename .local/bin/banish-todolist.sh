#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Wait for the Emacs daemon to start, open a new Emacs client frame for the specified file in a new process, move it to
# the specified workspace

# REQUIRES: bash, coreutils, emacs, firefox, grep, i3, jq, procps-ng

# ==== SHELL OPTIONS ====

set -euxo pipefail

# ==== CONSTANTS ====

declare -r destination_workspace='2'

declare -r window_class_regexp='^emacs$'

declare -r todo_list=~/Documents/todo.org

# ==== CODE ====

# make sure the daemon is actually, y'know, running
processes=$(ps -f -u $EUID) # -f is "do full-format listing" and -u is "userlist"
until [[ $(grep --extended-regexp --count 'emacs --daemon$' <<< "${processes}") -ge 1 ]]; do
    sleep 0.1
    processes=$(ps -f -u $EUID)
done

emacsclient --create-frame "${todo_list}" &

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
