#!/bin/bash -x
set -e
yt-dlp -x --audio-format mp3 --prefer-ffmpeg -o '~/Downloads/Music/%(title)s.%(ext)s' "$*"
