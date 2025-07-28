#!/bin/bash
set -euxo pipefail

# creates symbolic links from ~.config/userChrome.css~ to your Firefox's default profiles' ~/chrome/~ directories
#
# and copies ~colors.css~ from wal cache to the same

# make sure said userChrome is real
if [[ ! -f ~/.config/userChrome.css ]]; then
    printf "Unable to locate userChrome.css in your config directory, aborting!\n" 1>&2
    exit 1
fi

# parse ~profiles.ini~
if [[ ! -f ~/.mozilla/firefox/profiles.ini ]]; then
    printf "Unable to locate firefox profiles.ini, aborting!\n" 1>&2
    exit 1
fi
profiles=($(awk -f "$(dirname $0)"'/get-default-ff-profiles.awk' ~/.mozilla/firefox/profiles.ini))

# go through 'em all and run da script!
for profile in "${profiles[@]}"; do
    profile_dir=~/.mozilla/firefox/"${profile}"

    if [[ ! -d "${profile_dir}" ]]; then
        printf "Parsed profile dir ${profile_dir} does not exist, aborting!\n" 1>&2
        exit 1
    fi

    chrome_dir="${profile_dir}/chrome"

    if [[ ! -d "${chrome_dir}" ]]; then
        mkdir "${chrome_dir}"
    fi

    # if it exists, and isn't a symbolic link pointing where we want it, abort!
    if [[ -e "${chrome_dir}/userChrome.css" ]]; then
        if [[ ! ( -L "${chrome_dir}/userChrome.css"\
                      && "$(realpath --canonicalize-existing "${chrome_dir}/userChrome.css")"\
                = "$(realpath --canonicalize-existing ~/.config/userChrome.css)" ) ]]; then
            printf "userChrome.css already exists for profile ${profile}, aborting!\n" 1>&2
            exit 1
        fi
    else
        ln -s ~/.config/userChrome.css "${chrome_dir}/userChrome.css"
    fi

    # copy colors.css
    [[ -f ~/.cache/wal/colors.css ]]
    if [[ -f "${chrome_dir}/colors.css" ]]; then
        rm "${chrome_dir}/colors.css"
    elif [[ -L "${chrome_dir}/colors.css" ]]; then
        unlink "${chrome_dir}/colors.css"
    else
        exit 1
    fi
    cp ~/.cache/wal/colors.css "${chrome_dir}/colors.css"
done
