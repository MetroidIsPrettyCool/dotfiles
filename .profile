PATH="$PATH:$HOME/.local/bin"

TIGCC=$HOME/tigcc
PATH="$PATH:$TIGCC/bin"
ZCCCFG="/usr/share/z88dk/lib/config"

PATH="$PATH:$HOME/CEdev/bin"

MY_SCRIPTS_DIR=$HOME/Documents/scripts

ALTERNATE_EDITOR=""
EDITOR="emacsclient -t"
VISUAL="emacsclient -c -a emacs"
SUDO_EDITOR="/bin/emacs"

# no memory of why i thought i needed this or if it worked. leaving it for now...
SDL_GAMECONTROLLERCONFIG="0300000057696920552047616d654300,Wii U GameCube Adapter Port 1,platform:Linux,x:b3,a:b0,b:b1,y:b2,start:b7,dpleft:b10,dpdown:b9,dpright:b11,dpup:b8,lefttrigger:a2,rightshoulder:b6,righttrigger:a5,leftx:a0,lefty:a1,rightx:a3,righty:a4,"

export PATH TIGCC ZCCFG MY_SCRIPTS_DIR VISUAL EDITOR ALTERNATE_EDITOR SUDO_EDITOR SDL_GAMECONTROLLERCONFIG
