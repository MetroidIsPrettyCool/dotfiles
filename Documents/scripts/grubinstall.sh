#!/bin/bash -x
set -e

sudo grub-install --target=i386-pc /dev/sda
sudo grub-mkconfig -o /boot/grub/grub.cfg
