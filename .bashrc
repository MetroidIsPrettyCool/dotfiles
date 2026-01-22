#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# REQUIRES: bash, lolcat, $MY_SCRIPTS_DIR/my-cowfortune.sh

# If not running interactively, don't do anything.

if [[ ! "${-}" =~ i ]]; then
    return
fi

# ==== NODE VERSION MANAGER ====
source /usr/share/nvm/init-nvm.sh

# ==== SHELL OPTIONS ====

declare -x HISTFILE=~/.bash_history
declare -x HISTCONTROL=ignoredups
declare -x HISTFILESIZE=100000
declare -x HISTSIZE=100000

shopt -s autocd # INREQ: Emacs shell-mode struggles to interpret this, can we work around or should I switch this off?
shopt -s globstar # /**/*

# # ==== STERRED ====

# # Color stderr messages bold, italic, and red. I flip-flop on whether I like this or not. Requires libstderr to be
# # installed.

# export LD_PRELOAD="/usr/lib64/libstderred.so${LD_PRELOAD:+:$LD_PRELOAD}"
# export STDERRED_ESC_CODE="$(tput bold)$(tput sitm)$(tput setaf 1)"

# ==== PROMPT ====

# Let's borrow the "exit code in brackets" idea from the default eshell prompt.

# I like a long prompt with lots of info, but don't like when it makes my commands wrap around the terminal. We'll
# newline between the "current status" and the dollar sign, then.

__do_prompt_vars() {
    local EXIT_CODE="${?}"

    PS1=""

    # history number, green
    PS1+="\[\e[32m\]\!"

    # current time, blue
    PS1+="\[\e[34m\] \t"

    # user@host, white
    PS1+="\[\e[37m\] \u@\h"

    # basename of the current directory (w/ home abbreviated to a tilde), blue
    PS1+="\[\e[34m\] \W"

    # error code of the last process, if it failed, in brackets; light red
    if [[ $EXIT_CODE != 0 ]]; then
        PS1+="\[\e[91m\] [${EXIT_CODE}]"
    fi

    # dollar prompt, on new line, green, reset colors, space after prompt
    PS1+="\[\e[32m\]\n$\[\e[0m\] "
}

PROMPT_COMMAND=__do_prompt_vars

# ==== ALIASES ====

# Kept in a separate file for convenience's sake.

if [[ -f ~/.bash_aliases ]]; then
    source ~/.bash_aliases
fi

# ==== "MOTD" ====

# Emacs used to balk at ANSI 256 or 24-bit color stuff, so we had to test -v INSIDE_EMACS and skip the lolcat if we
# were. As of Emacs 29.1, the ANSI color filters were updated to support additional colors and this isn't necessary.
# Hooray!

if [[ ! $(tty) = /dev/tty1 ]]; then
    "${MY_SCRIPTS_DIR}"/my-cowfortune.sh | lolcat
fi
