# Setup my paths
set_my_paths

# Export everything that we need for wildfire
init_wildfire

# Setup rbenv
set -x PATH $HOME/.rbenv/bin $PATH
set -x PATH $HOME/.rbenv/shims $PATH
rbenv rehash >/dev/null ^&1

# Some universal variables
set -U EDITOR vim
set -U BROWSER google-chrome
set -U GOPATH $HOME/code/go
