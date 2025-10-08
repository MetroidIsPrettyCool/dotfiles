#!/bin/bash
set -euxo pipefail
yt-dlp -x --audio-format mp3 --prefer-ffmpeg --cookies-from-browser firefox -o '~/Downloads/Music/%(title)s.%(ext)s' "$*"
