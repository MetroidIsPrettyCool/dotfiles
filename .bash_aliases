alias ls='ls --color=auto'
alias la='ls -A'

alias paclean='sudo pacman -R $(pacman -Qdtq)'

function cl () {
    cd "$@" && ls
}

alias emacsc='emacsclient -nw'

alias emacs='emacs -nw'

alias semacs='sudo emacs -nw'

cowfortune () {
    COW=`ls /usr/share/cows/*.cow | shuf -n1`
    fortune -a | cowsay -n -f $COW
    echo `basename $COW .cow`
}

alias tor=torbrowser-launcher

alias java8=/usr/lib/jvm/java-8-openjdk/bin/java
alias javac8=/usr/lib/jvm/java-8-openjdk/bin/javac

extract () {
    if [ -f $1 ] ; then
	case $1 in
	    *.tar.bz2)   tar xvjf $1    ;;
	    *.tar.gz)    tar xvzf $1    ;;
	    *.bz2)       bunzip2 $1     ;;
	    *.rar)       unrar x $1     ;;
	    *.gz)        gunzip $1      ;;
	    *.tar)       tar xvf $1     ;;
	    *.tbz2)      tar xvjf $1    ;;
	    *.tgz)       tar xvzf $1    ;;
	    *.zip)       unzip $1       ;;
	    *.Z)         uncompress $1  ;;
	    *.7z)        7z x $1        ;;
	    *)           echo "don't know how to extract '$1'..." ;;
	esac
    else
	echo "'$1' is not a valid file!"
    fi
}

alias alsamixer='alsamixer -c 0'

alias pacman='sudo pacman'

alias ping='ping 8.8.8.8'

alias dc='cd'

alias ytmp3="youtube-dl -x --audio-format mp3 --prefer-ffmpeg -o '~/Downloads/Music/%(title)s.%(ext)s'"

alias gitdots='/usr/bin/git --git-dir=$HOME/.dots --work-tree=$HOME'
