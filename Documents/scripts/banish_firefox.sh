#!/bin/bash -x
set -e

firefox &

# wait for the window before trying to move it
until [[ $(i3-msg -t subscribe '[ "window" ]' \
              | grep --count '{.*"change":"new".*"container":{.*"name":"Mozilla Firefox".*}.*}') -ge 1 ]]; do
    sleep 0.1
done

i3-msg --quiet '[class="firefox"] focus'
i3-msg --quiet 'move window to workspace 1'

sleep 0.2

firefox -new-tab https://my.nintendo.com/
