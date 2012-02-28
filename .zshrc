# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="sporty_256"             # ***
#ZSH_THEME="sunrise"               # *
#ZSH_THEME="blinks"                # *
#ZSH_THEME="xiong-chiamiov-plus"   # ***
#ZSH_THEME="wedisagree"            # **
#ZSH_THEME="smt"                   # ***

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
# ssh-agent gpg-agent
plugins=(npm git github git-flow cap cloudapp extract gem gas python redis-cli thor ruby bundler rails3 history vi-mode vagrant history-substring-search lol heroku zsh_reload my_aliases)

if [ "`uname`" = "Darwin" ]; then
  plugins=(${plugins} osx brew pow)
fi

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
source $HOME/.shells/colors
source $HOME/.shells/exports
source $HOME/.shells/functions
source $HOME/.shells/lang
source $HOME/.shells/opts

# Python
source /Users/wael/.pythonbrew/etc/bashrc

# Rbenv
eval "$(rbenv init -)"

# Tmuxinator
[[ -s $HOME/.tmuxinator/scripts/tmuxinator ]] && source $HOME/.tmuxinator/scripts/tmuxinator
