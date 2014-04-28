# Add my paths
if [[ -x /usr/libexec/path_helper ]]; then
  eval `/usr/libexec/path_helper -s`
fi
typeset -Ug path # Make sure the path array does not contain duplicates
path+=(~/.filesystem/bin(N-/))
path+=(~/code/go/bin(N-/))
for i in ~/.filesystem/opt/*; do
  path+=($i/bin(N-/))
done

# Load Google specific stuff
if [ -r "$HOME/.zshrc-google" ]; then
  source "$HOME/.zshrc-google"
fi

# /brew ? Export DYLD path
if [[ -d /brew ]]; then
  path=(/brew/bin $path)
  export DYLD_LIBRARY_PATH=/brew/lib:$DYLD_LIBRARY_PATH
fi

# Load rbenv
if [[ -d ~/.rbenv ]]; then
  path=(~/.rbenv/bin(N-/) $path)
  path=(~/.rbenv/shims(N-/) $path)
  eval "$(rbenv init --no-rehash - zsh)"
fi

# Load antigen
source ~/.antigen/antigen.zsh

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

antigen bundles <<EOBUNDLES
  # Load my custom plugins
  $HOME/.oh-my-zsh-ext/colors
  $HOME/.oh-my-zsh-ext/my-functions
  $HOME/.oh-my-zsh-ext/my-exports
  $HOME/.oh-my-zsh-ext/my-aliases
  $HOME/.oh-my-zsh-ext/fix-vi-mode-on-debian
  $HOME/.oh-my-zsh-ext/mysql-credentials
  $HOME/.oh-my-zsh-ext/my-git-extensions

  # Syntax highlighting bundle.
  zsh-users/zsh-syntax-highlighting

  # Bundles from the default repo (robbyrussell's oh-my-zsh).
  command-not-found
  zsh_reload
  npm
  git
  github
  git-flow
  capistrano
  cloudapp
  extract
  gem
  python
  redis-cli
  thor
  ruby
  bundler
  rails
  vi-mode
  history
  history-substring-search
EOBUNDLES

# Load the theme.
antigen theme crunch

# Tell antigen that you're done.
antigen apply

# Export all my env variables
export GOPATH=$HOME/code/go

# create the pane with irssi's nicklist
function irssi_nickpane() {
    tmux renamew irssi                                              # name the window
    tmux -q setw main-pane-width $(( $(tput cols) - 21))            # set the main pane width to the total width-20
    tmux splitw -v "cat ~/.irssi/nicklistfifo"                      # create the window and begin reading the fifo
    tmux -q selectl main-vertical                                   # assign the layout
    tmux selectw -t irssi                                           # select window 'irssi'
    tmux selectp -t 0                                               # select pane 0
}

# irssi wrapper
function irssi() {
    irssi_nickpane
    /usr/bin/env irssi
}

# repair running irssi's nicklist pane
function irssi_repair() {
    tmux selectw -t irssi
    tmux selectp -t 0
    tmux killp -a                                                   # kill all panes
    irssi_nickpane
}

if [[ -s "${HOME}/.nvm/nvm.sh" ]]; then
  source "${HOME}/.nvm/nvm.sh"
fi
if [[ -s "${HOME}/.nvm/bash_completion" ]]; then
  source "${HOME}/.nvm/bash_completion"
fi

export PATH=./node_modules/.bin:$PATH
