#!/bin/sh

# This /should/ all be POSIX sh compatible

# ======== HELPERS ========

add_to_path() {
    # I know I borrowed this from somewhere, so sorry that I can't remember. I hope I've modified it enough to make it
    # my own...

    d="$(printf '%s\n' "$1" | sed -E '$s/\/$//')" # *sigh* no here-strings....
    [ -d "$d" ] && ! printf '%s\n' "$PATH" | grep -q '\(^\|:\)'"$d"'\(:\|$\)' && PATH="$PATH:$d"
}

# ======== PATH ========

# ==== XDG COMPLIANCE ====

# yadda yadda systemd XDG Base Directory spec. yadda yadda

add_to_path "$HOME/.local/bin"

# ==== TIGCC ====
TIGCC=$HOME/tigcc
add_to_path "$TIGCC/bin"

# ==== ZCC ====
ZCCCFG='/usr/share/z88dk/lib/config'

# ==== CEDEV ====
add_to_path "$HOME/CEdev/bin"

# ==== JAVA ====
JAVA_HOME='/usr/lib/jvm/default'

# ==== RUST ====
add_to_path "$HOME/.cargo/bin/"

# ======== ENVIRONMENT VARIABLES ========

MY_SCRIPTS_DIR=$HOME/Documents/scripts

EDITOR='/usr/bin/emacsclient --no-window-system'
ALTERNATE_EDITOR='/usr/bin/emacs --no-window-system'
VISUAL='/usr/bin/emacsclient --create-frame --alternate-editor=/usr/bin/emacs'
SUDO_EDITOR='/usr/bin/emacs'

# I have no memory of why I thought I needed this, /or/ if it worked. Just leaving it for now.
SDL_GAMECONTROLLERCONFIG='0300000057696920552047616d654300,Wii U GameCube Adapter Port 1,platform:Linux,x:b3,a:b0,b:b1,y:b2,start:b7,dpleft:b10,dpdown:b9,dpright:b11,dpup:b8,lefttrigger:a2,rightshoulder:b6,righttrigger:a5,leftx:a0,lefty:a1,rightx:a3,righty:a4,'

export PATH TIGCC ZCCCFG JAVA_HOME MY_SCRIPTS_DIR VISUAL EDITOR ALTERNATE_EDITOR SUDO_EDITOR SDL_GAMECONTROLLERCONFIG
