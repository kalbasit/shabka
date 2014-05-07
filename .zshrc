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

# Load pyenv
if [[ -d ~/.pyenv ]]; then
  path=(~/.pyenv/bin(N-/) $path)
  eval "$(pyenv init --no-rehash - zsh)"
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
  $HOME/.oh-my-zsh-ext/tmuxinator

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

# Options
setopt print_exit_value         # print return value if non-zero
unsetopt hist_beep              # no bell on error in history
unsetopt list_beep              # no bell on ambiguous completion

# Bell on every prompt
PROMPT+="%{$(echo "\a")%}"
