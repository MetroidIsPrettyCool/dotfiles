#!/bin/bash -x
set -e

# make sure the daemon is actually running
processes=$(ps -f -u $EUID) # -f is "do full-format listing" and -u is "userlist"
until [[ $(grep --count '/usr/bin/emacs --daemon' <<< "${processes}") -ge 1 ]]; do
    sleep 0.1
    processes=$(ps -f -u $EUID)
done

emacsclient -c ~/Documents/todo.org &

# wait for the window before trying to move it
until [[ $(i3-msg -t subscribe '[ "window" ]' \
              | grep --count '{.*"change":"new".*"container":{.*"name":".*GNU Emacs.*".*}.*}') -ge 1 ]]; do
    sleep 0.1
done

sleep 0.5

i3-msg --quiet '[class="Emacs"] focus'
i3-msg --quiet 'move window to workspace 2'
