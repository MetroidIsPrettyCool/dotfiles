#!/bin/bash
set -euxo pipefail

# make sure the daemon is actually running
processes=$(ps -f -u $EUID) # -f is "do full-format listing" and -u is "userlist"
until [[ $(grep --extended-regexp --count 'emacs --daemon$' <<< "${processes}") -ge 1 ]]; do
    sleep 0.1
    processes=$(ps -f -u $EUID)
done

emacsclient --create-frame ~/Documents/todo.org &

# wait for the window before trying to move it

subscribe_result="$(i3-msg -t subscribe '[ "window" ]')"
until [[ "$(jq '(.change == "new") and (.container.window_properties.class | test("^emacs$" ; "i"))' <<< "${subscribe_result}")" \
                                                                                                      = 'true' ]]; do
    subscribe_result="$(i3-msg -t subscribe '[ "window" ]')"
done

i3-msg --quiet '[class='"$(jq '.container.window_properties.class' <<< "${subscribe_result}")"'] focus'
i3-msg --quiet 'move window to workspace 2'
