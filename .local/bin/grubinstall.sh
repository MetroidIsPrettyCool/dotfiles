#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== DESCRIPTION ====

# Install Grub to the MBR of ~/dev/sda~

# REQUIRES: bash, grub

# ==== SHELL OPTIONS ====

set -euxo pipefail

# ==== CODE ====

sudo grub-install --target=i386-pc /dev/sda
sudo grub-mkconfig -o /boot/grub/grub.cfg
