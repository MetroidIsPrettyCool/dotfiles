#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Intended to be run after i3 starts to set the wallpaper, launch default applications, etc.

# REQUIRES: ./banish-firefox.sh, ./banish-todolist.sh, bash, blueberry, coreutils, discord, dunst, emacs, i3,
# ./launch-polybar.sh, ./launch-thunderbird.sh, netctl, pcmanfm, picom, solaar, ./themeback.sh, vlc, ./wlanrestart.sh,
# xcowfortune

# ==== SHELL OPTIONS ====

set -xuo pipefail

# ==== CONSTANTS ====

declare -r home_netctl_profile='trogdor'

# ==== CODE ====

# == VERIFY ALL IS WELL ==

# restart networking profile, if it's not up

# kinda hacky, might wanna rewrite this as a systemd unit or something
if ! netctl is-active "${home_netctl_profile}" &>/dev/null; then
    "${MY_SCRIPTS_DIR}"/wlanrestart.sh "${home_netctl_profile}"
fi

# == SET WALLPAPER AND GENERATE PROCEDURAL SYSTEM THEMES ==

# # select a random background image and move it to .cache/randback
# "${MY_SCRIPTS_DIR}"/randback.sh

# take whatever image is in .cache/randback and set it as wallpaper + generate system theme from it
"${MY_SCRIPTS_DIR}"/themeback.sh

# == START VARIOUS GUI-ONLY DAEMONS ==
emacs --daemon          # eeeeeeemacs
dunst &                 # notification daemon
solaar -w hide &        # Logitech I/O device daemon
blueberry-tray          # application for managing Bluetooth devices
pcmanfm --daemon-mode & # file manager daemon, for background volume management, mostly
picom &                 # compositor

# == XCOWFORTUNE :) ==
xcowfortune &

# == START SYSTEM INFORMATION BARS ==
"${MY_SCRIPTS_DIR}"/launch-polybar.sh &

# == START DEFAULT APPLICATIONS ==
vlc &                                       # for media, assigned to workspace 9 in my i3 config
discord &                                   # for IM, assigned to workspace 10 in my i3 config
"${MY_SCRIPTS_DIR}"/launch-thunderbird.sh & # for mail, assigned to workspace 8 in my i3 config

# We can only run one of these "banish" scripts at a time, as they need to have exclusive access to `i3-msg` to focus
# their eponymous applications and move them to their destination workspaces.

# TODO: figure out if I can just assign them to workspaces by class temporarily, and remove this exclusive access
# problem

# Emacs client with org-mode to-do list open, auto-moved to workspace 2 by the script
timeout 15s "${MY_SCRIPTS_DIR}"/banish-todolist.sh

# Firefox web browser, auto-moved to workspace 1 by the script
"${MY_SCRIPTS_DIR}"/banish-firefox.sh

# == FINAL TOUCHES ==
i3-msg --quiet 'workspace 1'
