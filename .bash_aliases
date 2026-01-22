#!/bin/bash

# Any copyright is dedicated to the Public Domain.
# http://creativecommons.org/publicdomain/zero/1.0/

# ==== INHIBIT DANGEROUS COMMANDS VIA ALIAS ====

alias rm='"${MY_SCRIPTS_DIR}"/rm-warning.sh'

# ==== DEFAULT ARGUMENTS ====

alias ls='/usr/bin/ls --color=auto'

alias ping='/usr/bin/ping 8.8.8.8'

alias alsamixer='/usr/bin/alsamixer -c 0'

# ==== ALTERNATE NAMES FOR COMMON ARGUMENTS ====

alias la='/usr/bin/ls --color=auto -A'
alias lal='/usr/bin/ls --color=auto -Al'

alias paclean='/usr/bin/sudo /usr/bin/pacman -R $(/usr/bin/pacman -Qdtq)'

alias yay-remove='/usr/bin/yay -Rns'

# everything but -help and -version
alias javap-all='/usr/bin/javap -l -private -s -sysinfo -constants -c -verbose'

# turns default allows into warns
alias clippy-all='/usr/bin/cargo clippy -- -W clippy::all \
                                           -W clippy::pedantic \
                                           -W clippy::restriction \
                                           -W clippy::nursery \
                                           -W clippy::cargo'

alias clippy-all-fix='/usr/bin/cargo clippy --fix -- -W clippy::all \
                                                     -W clippy::pedantic \
                                                     -W clippy::restriction \
                                                     -W clippy::nursery \
                                                     -W clippy::cargo'

# alias grub-mkconfig='grub-mkconfig -o /boot/grub/grub.cfg'

# ==== DOTFILE MANAGEMENT ====

dotfiles-git() (
    set -euxo pipefail

    local drd=~/.dotfiles-repo/ # dotfiles repository directory

    /usr/bin/git --git-dir="${drd}" --work-tree="${HOME}" "$@"
)

dotfiles-git-cloc() (
    set -euo pipefail

    cd ~

    readarray -t files < <(dotfiles-git ls-files)

    cloc "${files[@]}"
)

dotfiles-git-update() (
    set -euxo pipefail
    dotfiles-git add -u
    dotfiles-git add ~/.emacs.d/lisp/*.el
    dotfiles-git add ~/.emacs.d/snippets/
    dotfiles-git commit -m "update $(date --universal)"
    dotfiles-git push github master
)

# ==== SHORT NAMES FOR NON-PATH EXECUTABLES ====

alias java8=/usr/lib/jvm/jre1.8.0_451/bin/java
alias javac8=/usr/lib/jvm/java-8-openjdk/bin/javac

alias script-grubinstall='"${MY_SCRIPTS_DIR}"/grubinstall.sh'
alias script-ytmp3='"${MY_SCRIPTS_DIR}"/ytmp3.sh'
alias script-update-system='"${MY_SCRIPTS_DIR}"/update-system.sh'
alias script-manage-orphan-tilde-files='"${MY_SCRIPTS_DIR}"/manage-orphan-tilde-files.sh'

# ==== DIRECTORY NAVIGATION ====

cl() {
    cd "$@"
    ls --color=auto -Al
}

cdu() {
    if [[ ( $# -ne 1 ) || ( $1 = "-h" ) || ( $1 = "--help" ) ]]; then
        printf 'cdu -- change directory up to some parent.\nusage: cdu [PARENT]\n'
    fi

    if [[ -z ${PWD+t} ]]; then
        printf 'the $PWD environment variable is'\'' set, how'\''d that happen?\n'
    fi

    cd "${PWD%/${1%%/*}/*}/${1}" && pwd
}

clu() {
    cdu "${@}"
    ls --color=auto -Al
}
