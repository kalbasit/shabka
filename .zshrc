# Add my paths
typeset -Ug path # Make sure the path array does not contain duplicates
path+=(~/.filesystem/bin(N-/))
path+=(/brew/bin(N-/))
for i in ~/.filesystem/opt/*; do
  path+=($i/bin(N-/))
done

# Load rbenv
path+=(~/.rbenv/bin(N-/))
path+=(~/.rbenv/shims(N-/))
eval "$(rbenv init --no-rehash - zsh)"

# Load antigen
source ~/.antigen/antigen.zsh

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

antigen bundles <<EOBUNDLES
  # Load my custom plugins
  $HOME/.oh-my-zsh-ext/my-aliases
  $HOME/.oh-my-zsh-ext/zsh-reload
  $HOME/.oh-my-zsh-ext/fix-vi-mode-on-debian

  # Syntax highlighting bundle.
  zsh-users/zsh-syntax-highlighting

  # Bundles from the default repo (robbyrussell's oh-my-zsh).
  command-not-found
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
  rails3
  vi-mode
  history
  history-substring-search
EOBUNDLES

# Load the theme.
antigen theme crunch

# Tell antigen that you're done.
antigen apply
