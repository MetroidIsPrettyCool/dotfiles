#!/bin/bash

$HOME/scripts/alacritty-colors.sh
$HOME/scripts/dunst-colors.sh
/opt/oomox/plugins/theme_oomox/change_color.sh -o oomox-xresources-reverse $HOME/.config/oomox/scripted_colors/xresources/xresources-reverse
cp $HOME/.cache/wal/colors.Xresources $HOME/.Xresources
