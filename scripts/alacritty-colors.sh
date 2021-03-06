#!/bin/bash
# Alacritty Color Export
# Version 0.1.0
# github.com/egeesin
#
# Exports generated Wal colors to Alacritty config
# WARNING: Don't forget to backup your Alacritty config
# before execute this script!
#
# Dependencies: bash, grep, sed
# Usage: ./script.sh
#        ./script.sh <config yml>

# Get Wal color file
SRC=${HOME}"/.cache/wal/colors"
[[ ! -e $SRC ]] && exit 0

READLINK=$( command -v greadlink | command -v readlink )

# Get config file
if [[ $1 ]]; then
	# Check path syntax here
	[[ -e $1 ]] && CFG=$1
	[[ -L $1 ]] && CFG=$($READLINK -f $1)
	[[ ! -e $1 ]] && exit 0
else
	# Default config path
	[[ -e ${HOME}"/.config/alacritty/alacritty.yml" ]] && CFG=${HOME}"/.config/alacritty/alacritty.yml"
	[[ -L ${HOME}"/.config/alacritty/alacritty.yml" ]] && CFG=$($READLINK -f ${HOME}"/.config/alacritty/alacritty.yml")
	[[ ! -e $CFG ]] && exit 0
fi

# Get and convert hex colors from Wal cache
index=1
for line in $(grep -oE '[0-9a-fA-F]{6}' "$SRC")
do
	regex='[0-9a-fA-F]{6}'
	if [[ $line =~ $regex ]]
	then
		eval "color$index=0x$line"
		index=$((index+1));
	fi
done

# Delete existing color declerations (generated by this script)
if grep -q '^# BEGIN ACE' "$CFG"; then
	sed -i '' -e '/^# BEGIN ACE/,/^# END ACE/{/^# BEGIN ACE/!{/^# END ACE/!d;};}' "$CFG"
# ^^^ There's something wrong with this command. If BEGIN ACE comment
# doesn't exist but END ACE, just deletes all config except END ACE line.
# In the mean time, surrounding comments must not touched, until finding
# safer version of this command.
else
	echo -e '# BEGIN ACE\n# END ACE' >> "$CFG";
fi

# vvv This line may give error on different systems.
sed -i '' -e "/^# BEGIN ACE/ r /dev/stdin" "$CFG" <<EOF
colors:
  primary:
    background: '$color1'
    foreground: '$color8'
  cursor:
    text:       '$color1'
    cursor:     '$color8'
  normal:
    black:      '$color1'
    red:        '$color2'
    green:      '$color3'
    yellow:     '$color4'
    blue:       '$color5'
    magenta:    '$color6'
    cyan:       '$color7'
    white:      '$color8'
  bright:
    black:      '$color9'
    red:        '$color10'
    green:      '$color11'
    yellow:     '$color12'
    blue:       '$color13'
    magenta:    '$color14'
    cyan:       '$color15'
    white:      '$color16'

bell:
  animation: EaseOutExpo
  duration: 250
  color: '$color8'
EOF
