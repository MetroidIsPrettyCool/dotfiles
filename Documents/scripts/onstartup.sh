#!/bin/bash -x
set -e

# intended to be run immediately after X starts to set the wallpaper, launch default applications, etc.

# ==== VERIFY ALL IS WELL ====

# restart networking profile if it's not up
#
# kinda hacky, might wanna rewrite this as a systemd unit or something
home_netctl_profile='home-home'
if ! netctl is-active "${home_netctl_profile}" 1>/dev/null 2>/dev/null; then
    $MY_SCRIPTS_DIR/wlanrestart.sh "${home_netctl_profile}"
fi

# ==== SET WALLPAPER AND GENERATE PROCEDURAL SYSTEM THEMES ====

# # select a random background image and move it to .cache/randback
# $MY_SCRIPTS_DIR/randback.sh

# take whatever image is in .cache/randback and set it as wallpaper + generate system theme from it
$MY_SCRIPTS_DIR/themeback.sh

# ==== START VARIOUS GUI-ONLY DAEMONS ====
#
# TODO: learn if these can be moved to system units, do so if they can.
dunst & # notification daemon
solaar -w hide & # Logitech I/O device daemon
blueberry-tray # application for managing Bluetooth devices
pcmanfm --daemon-mode & # file manager daemon, for background volume management, mostly
picom & # compositor

# ==== XCOWFORTUNE :) ====
xcowfortune &

# ==== START SYSTEM INFORMATION BARS ====
$MY_SCRIPTS_DIR/launch_polybar.sh &

# ==== START DEFAULT APPLICATIONS ====
vlc & # for media, assigned to workspace 9 in my i3 config
discord & # for IM, assigned to workspace 10 in my i3 config
$MY_SCRIPTS_DIR/launch_thunderbird.sh & # for mail, assigned to workspace 8 in my i3 config

# we can only run one of these at a time, as they need to have exclusive access to `i3-msg` in order to move their
# eponymous applications to their destination workspaces without outright assigning their classes
#
# TODO: figure out if I can just assign them temporarily and remove this exclusive access problem

# Emacs client with org-mode TODO list open, auto-moved to workspace 2 by the script
timeout 15s $MY_SCRIPTS_DIR/banish_todolist.sh
# Firefox web browser, auto-moved to workspace 1 by the script
$MY_SCRIPTS_DIR/banish_firefox.sh

# ==== FINAL TOUCHES ====
i3-msg --quiet 'workspace 1'
