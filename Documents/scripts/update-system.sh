#!/bin/bash -x
set -e

yay -Syu

rustup update

# emacs --batch -f package-upgrade-all
echo "remember to check emacsclient to confirm or deny the upgrade!"
emacsclient --eval '(package-upgrade-all t)'
