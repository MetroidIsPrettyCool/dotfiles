# -*- mode: shell-script; -*-

# ==== SMALL FUNCTIONS ====
function cl {
    cd "$*" && ls -Al
}

# ==== DEFAULT ARGUMENTS ====
alias ls='ls --color=auto'

alias ping='ping 8.8.8.8'

alias alsamixer='alsamixer -c 0'

alias rm=$MY_SCRIPTS_DIR/rm_warning.sh

# ==== ALTERNATE NAMES FOR COMMON ARGUMENTS  ====
alias la='ls -A'
alias lal='ls -Al'

alias paclean='sudo pacman -R $(pacman -Qdtq)'

alias yay-remove='yay -Rns'

alias dotfiles-git="/usr/bin/git --git-dir=$HOME/.dotfiles-repo/ --work-tree=${HOME}"

# alias grub-mkconfig='grub-mkconfig -o /boot/grub/grub.cfg'

# ==== SHORT NAMES FOR NON-PATH EXECUTABLES ====
alias java8=/usr/lib/jvm/jre1.8.0_451/bin/java
alias javac8=/usr/lib/jvm/java-8-openjdk/bin/javac

alias script-grubinstall=$MY_SCRIPTS_DIR/grubinstall.sh
alias script-ytmp3=$MY_SCRIPTS_DIR/ytmp3.sh
alias script-update-system=$MY_SCRIPTS_DIR/update-system.sh
alias script-manage-orphan-tilde-files=$MY_SCRIPTS_DIR/manage_orphan_tilde_files.sh
