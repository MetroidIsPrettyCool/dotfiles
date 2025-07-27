#!/bin/bash
set -euxo pipefail
# not production tested! no warranty! etc.

wallpapers_dir=~/Pictures/wallpapers

# nullglob 'cause I read this:
# https://stackoverflow.com/questions/18884992/how-do-i-assign-ls-to-an-array-in-linux-bash/18887210#18887210
shopt -s nullglob
wallpaper_files=("${wallpapers_dir}/"*)
shopt -u nullglob

valid_wallpaper_mime_types=("image/png" "image/jpeg")

potential_wallpapers=()

# echo subshell is required to turn those newlines into spaces
for wallpaper_file in "${wallpaper_files[@]}"; do
    quoted_filename=$(printf "%q" ${wallpaper_file})

    # ignore wallpaper files with a name ending in "-disabled"
    if [[ ${quoted_filename} =~ -disabled$ ]]; then
        continue
    fi

    file_mime_type=$(xdg-mime query filetype "${wallpaper_file}")

    # ignore wallpaper files with invalid mime types
    if [[ ! " ${valid_wallpaper_mime_types[@]} " =~ [[:space:]]"${file_mime_type}"[[:space:]] ]]; then
        continue
    fi

    potential_wallpapers+=("${wallpaper_file}")
done

# make sure we've got at least one option
[[ ! -z "${potential_wallpapers[@]}" ]]

# select a random wallpaper
new_wallpaper="${potential_wallpapers[RANDOM % ${#potential_wallpapers[@]}]}"

# remove the previous wallpaper(s)
#
# we skip the rm command if the directory is empty, because we don't wanna trip over set -e
if [[ ! -z $(ls ~/.cache/randback) ]]; then
    rm --verbose ~/.cache/randback/*
fi

# copy the new wallpaper to a known location
cp "${new_wallpaper}" ~/.cache/randback/
