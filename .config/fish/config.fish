# Setup my paths
set_my_paths

# Export everything that we need for wildfire
init_wildfire

# Init bundler
load_bundler

# Setup rbenv
set -x PATH $HOME/.rbenv/bin $PATH
set -x PATH $HOME/.rbenv/shims $PATH
rbenv rehash >/dev/null ^&1

# Some global variables
set -xg EDITOR vim
set -xg BROWSER google-chrome
set -xg GOPATH $HOME/code/go
