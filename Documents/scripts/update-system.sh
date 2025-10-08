#!/bin/bash
set -euxo pipefail

yay -Syu

rustup update
cargo install-update -a

# emacs --batch -f package-upgrade-all
# echo "remember to check emacsclient to confirm or deny the upgrade!"
emacsclient --create-frame --no-window-system --eval '(package-upgrade-all t)'
