#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# yt-dlp with my usual flags preloaded.

# REQUIRES: bash, ffmpeg, firefox, yt-dlp

# ==== SHELL OPTIONS ====

set -euxo pipefail

# ==== CODE ====

yt-dlp -x \
       --audio-format mp3 \
       --prefer-ffmpeg \
       --cookies-from-browser firefox \
       -o "${HOME}"'/Downloads/Music/%(title)s.%(ext)s' \
       "$*"
