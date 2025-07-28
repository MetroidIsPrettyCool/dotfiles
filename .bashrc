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

# export LD_PRELOAD="/usr/lib64/libstderred.so${LD_PRELOAD:+:$LD_PRELOAD}"
# bold=$(tput bold)
# italics=$(tput setab 7)
# export STDERRED_ESC_CODE=`echo -e "$(tput bold)$(tput sitm)$(tput setaf 1)$(tput setab 7)"`

if [[ -f ~/.bash_aliases ]]; then
    source ~/.bash_aliases
fi

if [[ ! ( -v INSIDE_EMACS || $(tty) = /dev/tty1 ) ]]; then
    $MY_SCRIPTS_DIR/my-cowfortune.sh | lolcat
elif [[ -v INSIDE_EMACS ]]; then
    $MY_SCRIPTS_DIR/my-cowfortune.sh
fi

# [[ ! -v INSIDE_EMACS ]] && printf "\a"
