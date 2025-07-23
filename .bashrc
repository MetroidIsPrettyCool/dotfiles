# ~/.bashrc

# if not running interactively, don't do anything
if [[ ! "${-}" =~ i ]]; then
    return
fi

PS1="\[\e[32m\]\!\[\e[34m\] \t \[\e[37m\]\u@\h \[\e[34m\]\W\n\[\e[32m\]$\[\e[37m\] "

declare -x HISTFILE=~/.bash_history
declare -x HISTCONTROL=ignoredups
declare -x HISTFILESIZE=100000
declare -x HISTSIZE=100000

shopt -s autocd
shopt -s globstar

export TIGCC=~/tigcc
export PATH="$PATH:$TIGCC/bin"
export ZCCCFG="/usr/share/z88dk/lib/config"

export PATH="$PATH:${HOME}/CEdev/bin"

export MY_SCRIPTS_DIR=~/Documents/scripts

export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"
export VISUAL="emacsclient -c -a emacs"
export SUDO_EDITOR="/bin/emacs"

# no memory of why i thought i needed this or if it worked. leaving it for now...
export SDL_GAMECONTROLLERCONFIG="0300000057696920552047616d654300,Wii U GameCube Adapter Port 1,platform:Linux,x:b3,a:b0,b:b1,y:b2,start:b7,dpleft:b10,dpdown:b9,dpright:b11,dpup:b8,lefttrigger:a2,rightshoulder:b6,righttrigger:a5,leftx:a0,lefty:a1,rightx:a3,righty:a4,"

# export LD_PRELOAD="/usr/lib64/libstderred.so${LD_PRELOAD:+:$LD_PRELOAD}"
# bold=$(tput bold)
# italics=$(tput setab 7)
# export STDERRED_ESC_CODE=`echo -e "$(tput bold)$(tput sitm)$(tput setaf 1)$(tput setab 7)"`

if [[ -f ~/.bash_aliases ]]; then
    source ~/.bash_aliases
fi

if [[ ! ( -v INSIDE_EMACS || $(tty) = /dev/tty1 ) ]]; then
    $MY_SCRIPTS_DIR/my_cowfortune.sh | lolcat
elif [[ -v INSIDE_EMACS ]]; then
    $MY_SCRIPTS_DIR/my_cowfortune.sh
fi

# [[ ! -v INSIDE_EMACS ]] && printf "\a"
