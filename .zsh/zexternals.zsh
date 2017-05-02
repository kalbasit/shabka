if [[ -x $(which brew 2>/dev/null) ]]; then
  # Load autojump
  [[ -s "$(brew --prefix)/etc/profile.d/autojump.sh" ]] && source "$(brew --prefix)/etc/profile.d/autojump.sh"

  # Export CFLAGS and LDFLAGS
  export CGO_CFLAGS="-I/usr/local/include"
  export CGO_CPPFLAGS="${CGO_CFLAGS}"
  export CGO_CXXFLAGS="${CGO_CFLAGS}"
  export CGO_LDFLAGS="-L/usr/local/lib"
fi

# Load TheFuck
[[ -x "$(which thefuck 2>/dev/null)" ]] && eval "$(thefuck --alias)"

# Load iterm2 shell integration
[[ -r "${HOME}/.iterm2_shell_integration.zsh" ]] && source "${HOME}/.iterm2_shell_integration.zsh"

# Load travis
[[ -r "${HOME}/.travis/travis.sh" ]] && source "${HOME}/.travis/travis.sh"

# Load FZF
[[ -f "${HOME}/.fzf.zsh" ]] && source "${HOME}/.fzf.zsh"

# Load SSH agents
[[ -x "${HOME}/.bin/ssh-agents" ]] && eval `ssh-agents $SHELL`
#
# Load rbenv
if [[ -d "${HOME}/.rbenv" ]]; then
  pathmunge "${HOME}/.rbenv/bin"
  eval "$(rbenv init --no-rehash -)"
fi

# Load pyenv
if [[ -d "${HOME}/.pyenv" ]]; then
  pathmunge "${HOME}/.pyenv/bin"
  eval "$(pyenv init --no-rehash -)"
fi

# Load nvm
if [[ -f "/usr/share/nvm/init-nvm.sh" ]]; then
  nvm_init="/usr/share/nvm/init-nvm.sh"
elif [[ -f "${HOME}/.nvm/nvm.sh" ]]; then
  nvm_init="${HOME}/.nvm/nvm.sh"
fi
if [[ -n "${nvm_init}" ]]; then
  if [[ -z "${PUBLICA_NPM_TOKEN}" ]]; then
    # set the PUBLICA_NPM_TOKEN to a bogus value, it will be loaded by the
    # publica profile when it gets loaded
    export PUBLICA_NPM_TOKEN="undefined"
  fi
  source "${nvm_init}"

  # if a folder contains an .nvmrc, respect it
  autoload -U add-zsh-hook
  add-zsh-hook chpwd load_nvmrc
  load_nvmrc
fi
unset nvm_init

# load k8s completion
if [[ -x $(which kubectl 2>/dev/null) ]]; then
  source <(kubectl completion zsh)
fi

# load the Emscripten environment
if [[ -d "/usr/lib/emsdk" ]]; then
  pathmunge "/usr/lib/emsdk"
fi
