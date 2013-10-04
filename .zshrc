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
