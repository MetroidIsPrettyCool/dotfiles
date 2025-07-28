#!/bin/bash
set -euxo pipefail

firefox &

# wait for the window before trying to move it
subscribe_result="$(i3-msg -t subscribe '[ "window" ]')"
until [[ "$(jq '(.change == "new") and (.container.window_properties.class | test("^firefox$|^navigator$" ; "i"))' <<< "${subscribe_result}")" \
                                                                                                      = 'true' ]]; do
    subscribe_result="$(i3-msg -t subscribe '[ "window" ]')"
done

i3-msg --quiet '[class='"$(jq '.container.window_properties.class' <<< "${subscribe_result}")"'] focus'
i3-msg --quiet 'move window to workspace 1'

sleep 0.2

firefox -new-tab https://my.nintendo.com/
