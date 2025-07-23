#!/bin/bash -x
set -e

# set wallpaper
# feh --bg-scale ~/.cache/randback/*

# # set wallpaper and generate most theme files
wal -i ~/.cache/randback/*

# # generate GTK theme
# /opt/oomox/plugins/theme_oomox/change_color.sh -o oomox-xresources-reverse ~/.config/oomox/scripted_colors/xresources/xresources-reverse

# copy generated files to their destinations
[[ -f ~/.cache/wal/alacritty.toml ]]
cp --force ~/.cache/wal/alacritty.toml ~/.config/alacritty/alacritty.toml

[[ -f ~/.cache/wal/colors.Xresources ]]
cp --force  ~/.cache/wal/colors.Xresources ~/.Xresources
xrdb -merge ~/.Xresources

[[ -f ~/.cache/wal/dunstrc ]]
cp  --force ~/.cache/wal/dunstrc ~/.config/dunst/dunstrc

# ensure Firefox userChrome.css has been copied
$MY_SCRIPTS_DIR/copy-userchrome.sh
