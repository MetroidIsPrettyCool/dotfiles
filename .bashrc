# ~/.bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1="\[\e[32m\]\!\[\e[34m\] (\t) \[\e[37m\][\u@\h \[\e[34m\]\W\[\e[37m\]]\[\e[32m\]\$\[\e[37m\] "

declare -x HISTFILE=~/.bash_history
declare -x HISTCONTROL=ignoredups
declare -x HISTFILESIZE=100000
declare -x HISTSIZE=100000

shopt -s autocd

export PATH="/home/joseph/.local/tigcc/bin:/home/joseph/scripts:/home/joseph/.local/bin:/usr/games:$PATH"

export SDL_GAMECONTROLLERCONFIG="0300000057696920552047616d654300,Wii U GameCube Adapter Port 1,platform:Linux,x:b3,a:b0,b:b1,y:b2,start:b7,dpleft:b10,dpdown:b9,dpright:b11,dpup:b8,lefttrigger:a2,rightshoulder:b6,righttrigger:a5,leftx:a0,lefty:a1,rightx:a3,righty:a4,"

export EDITOR="/bin/emacs"
export VISUAL="/bin/emacs"
export SUDO_EDITOR="/bin/emacs"

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

[[ ! -v INSIDE_EMACS ]] && cowfortune | lolcat

[[ -v INSIDE_EMACS ]] && cowfortune

[[ ! -v INSIDE_EMACS ]] && printf "\a"
