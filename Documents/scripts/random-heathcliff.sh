#!/bin/bash -x
set -e
# no longer works 'cause gocomics are all evil greedy bastards and forbade archive access to nonpaying members
#
# leaving this here as a memorial :(

now=$(date +%s)

# 1009843200 is 2002-01-01 @ 00:00:00 UTC
random_date=$(shuf -i 1009843200-$now -n 1)

#e.g. https://www.gocomics.com/heathcliff/2006/10/30
xdg-open $(date -u --date=@$random_date '+https://www.gocomics.com/heathcliff/%Y/%m/%d')
