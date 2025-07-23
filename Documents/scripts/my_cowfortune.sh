#!/bin/bash
set -e

# TODO: more cowfiles!
cowfiles=(/usr/share/cowsay/cows/*)

cow=${cowfiles[RANDOM % ${#cowfiles[@]}]}

fortune -a | cowsay -n -f $cow

echo $(basename $cow .cow)
