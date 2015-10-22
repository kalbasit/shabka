if [[ -x $(which brew 2>/dev/null) ]]; then
  # Load autojump
  [[ -s "$(brew --prefix)/etc/profile.d/autojump.sh" ]] && source "$(brew --prefix)/etc/profile.d/autojump.sh"

  # Export CFLAGS and LDFLAGS
  export CGO_CFLAGS="-I/usr/local/include"
  export CGO_CPPFLAGS="${CGO_CFLAGS}"
  export CGO_CXXFLAGS="${CGO_CFLAGS}"
  export CGO_LDFLAGS="-L/usr/local/lib"
fi

# Load travis
[[ -r "${HOME}/.travis/travis.sh" ]] && source "${HOME}/.travis/travis.sh"

# Load FZF
[[ -f "${HOME}/.fzf.zsh" ]] && source "${HOME}/.fzf.zsh"

# Load SSH agents
[[ -x "${HOME}/.bin/ssh-agents" ]] && eval `ssh-agents $SHELL`
