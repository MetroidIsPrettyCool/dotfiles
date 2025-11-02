#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Set the wallpaper based on whatever randback spat out, automatically generate themes for different applications with
# them.

# REQUIRES: bash, ./copy-userchrome.sh, coreutils, oomox, wal

# ==== SHELL OPTIONS ====

set -euxo pipefail

# ==== CODE ====

# == GENERATE THEMES ==

# # set wallpaper
# feh --bg-scale ~/.cache/randback/*

# set wallpaper and generate most of the theme files
wal -i ~/.cache/randback/*

# # generate GTK theme
# /opt/oomox/plugins/theme_oomox/change_color.sh -o oomox-xresources-reverse ~/.config/oomox/scripted_colors/xresources/xresources-reverse

# # copy generated files to their destinations
# [[ -f ~/.cache/wal/alacritty.toml ]]
# if [[ -f ~/.config/alacritty/alacritty.toml ]]; then
#     rm ~/.config/alacritty/alacritty.toml
# elif [[ -L ~/.config/alacritty/alacritty.toml ]]; then
#     unlink ~/.config/alacritty/alacritty.toml
# else
#     exit 1
# fi
# cp ~/.cache/wal/alacritty.toml ~/.config/alacritty/alacritty.toml

# == COPY FILES ==

[[ -f ~/.cache/wal/colors.Xresources ]]
if [[ -f ~/.Xresources ]]; then
    rm ~/.Xresources
elif [[ -L ~/.Xresources ]]; then
    unlink ~/.Xresources
else
    exit 1
fi
cp --force  ~/.cache/wal/colors.Xresources ~/.Xresources
xrdb -merge ~/.Xresources

[[ -f ~/.cache/wal/dunstrc ]]
if [[ -f ~/.config/dunst/dunstrc ]]; then
    rm ~/.config/dunst/dunstrc
elif [[ -L ~/.config/dunst/dunstrc ]]; then
    unlink ~/.config/dunst/dunstrc
else
    exit 1
fi
cp  --force ~/.cache/wal/dunstrc ~/.config/dunst/dunstrc

# ensure Firefox userChrome.css has been copied
"${MY_SCRIPTS_DIR}"/copy-userchrome.sh
