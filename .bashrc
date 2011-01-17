# Bashrc made by Wael Nasreddine
# Some of the function and tips were taken from phrakture
# http://phraktured.net/config/.bashrc

# History is in .bash/history!
export HISTFILE=~/.bash/history

# Include aliases, export, function, etc...
source ${HOME}/.shells/colors
source ${HOME}/.shells/exports
source ${HOME}/.shells/alias
source ${HOME}/.shells/functions
source ${HOME}/.shells/opts
source ${HOME}/.bash/opts

# Which prompt to use?
USE_PROMPT="wael"
source ${HOME}/.bash/prompts/${USE_PROMPT}
unset USE_PROMPT

# bash_completion
# pacman -S bash-completion
[ -x /etc/profile.d/bash-completion ] && source /etc/profile.d/bash-completion

# Commented coz it hurts my eyes, need to play with it a bit to get a better
# color specially for the background !!
#if [ "${TERM}" == "linux" -a `id -u` -ne 0 ]; then
#    echo -en "\e]P0222222" #black
#    echo -en "\e]P8222222" #darkgrey
#    echo -en "\e]P1803232" #darkred
#    echo -en "\e]P9982b2b" #red
#    echo -en "\e]P25b762f" #darkgreen
#    echo -en "\e]PA89b83f" #green
#    echo -en "\e]P3aa9943" #brown
#    echo -en "\e]PBefef60" #yellow
#    echo -en "\e]P4324c80" #darkblue
#    echo -en "\e]PC2b4f98" #blue
#    echo -en "\e]P5706c9a" #darkmagenta
#    echo -en "\e]PD826ab1" #magenta
#    echo -en "\e]P692b19e" #darkcyan
#    echo -en "\e]PEa1cdcd" #cyan
#    echo -en "\e]P7ffffff" #lightgrey
#    echo -en "\e]PFdedede" #white

#    #this is an attempt at working utf8 line drawing chars in the lippe-console
#    export TERM=lippe+utf8
#    clear #hmm, yeah we need this or else we get funky background collisions
#fi
