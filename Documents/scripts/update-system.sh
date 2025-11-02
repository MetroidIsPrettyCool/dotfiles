#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Run various update commands.

# REQUIRES: bash, cargo-install-update, emacs, rustup, yay

# ==== SHELL OPTIONS ====

set -euxo pipefail

# ==== CODE ====

yay -Syu

rustup update
cargo install-update -a

# emacs --batch -f package-upgrade-all
# echo "remember to check emacsclient to confirm or deny the upgrade!"
emacsclient --create-frame --no-window-system --eval '(package-upgrade-all t)'
