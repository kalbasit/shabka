typeset -Ug path # Make sure the path array does not contain duplicates
[[ -x /usr/libexec/path_helper ]] && eval `/usr/libexec/path_helper -s`
if [[ -d $HOME/.filesystem/bin ]]; then
  path+=($HOME/.filesystem/bin(N-/))
  export DYLD_LIBRARY_PATH=$HOME/.filesystem/bin:$DYLD_LIBRARY_PATH
fi
path+=($HOME/code/go/bin(N-/))
for i in $HOME/.filesystem/opt/*; do
  path+=($i/bin(N-/))
done
for i in $HOME/.filesystem/opt/*; do
  export DYLD_LIBRARY_PATH=$i/lib:$DYLD_LIBRARY_PATH
done
if [[ -d /brew ]]; then
  path=(/brew/bin $path)
  export DYLD_LIBRARY_PATH=/brew/lib:$DYLD_LIBRARY_PATH
fi

# Load Google specific stuff
[[ -r "$HOME/.zshrc-google" ]] && source "$HOME/.zshrc-google"

# Load travis
[[ -r "$HOME/.travis/travis.sh" ]] && source "$HOME/.travis/travis.sh"

# Load rbenv
if [[ -d $HOME/.rbenv ]]; then
  path=($HOME/.rbenv/bin(N-/) $path)
  eval "$(rbenv init --no-rehash - zsh)"
fi

# Load pyenv
if [[ -d $HOME/.pyenv ]]; then
  path=($HOME/.pyenv/bin(N-/) $path)
  eval "$(pyenv init --no-rehash - zsh)"
fi

# Load antigen
source "$HOME/.antigen/antigen.zsh"

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

antigen bundles <<EOBUNDLES
  # Load my custom plugins
  $HOME/.oh-my-zsh-ext/colors
  $HOME/.oh-my-zsh-ext/my-functions
  $HOME/.oh-my-zsh-ext/my-exports
  $HOME/.oh-my-zsh-ext/my-aliases
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
  extract
  python
  redis-cli
  bundler
  rails
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
