if [[ -z "${ENV_INIT}" ]]; then
  export ENV_INIT=true
  export EDITOR=vim

  if [[ -d "${HOME}/.filesystem" ]]; then
    export MYFS="${HOME}/.filesystem"
    export PATH="${HOME}/.bin:${MYFS}/bin:$PATH"

    if [[ -d "${MYFS}/opt" ]]; then
      if [[ -d "${MYFS}/opt/go_appengine" ]]; then
        export PATH="${MYFS}/opt/go_appengine:${PATH}"
      fi

      for dir in `ls --color=never "${MYFS}/opt"`; do
        if [[ -d "${MYFS}/opt/${dir}/bin" ]]; then
          export PATH="${MYFS}/opt/${dir}/bin:${PATH}"
        fi
      done
    fi
  fi

  if [[ -d "${HOME}/go" ]]; then
    export GOPATH="${HOME}/go:${GOPATH}"
    export PATH="${HOME}/go/bin:${PATH}"
  fi

  if [[ -d "${HOME}/.cask/bin" ]]; then
    export PATH="${HOME}/.cask/bin:${PATH}"
    export CASK_PATH="$(dirname $(dirname $(which cask)))"
  fi

  # Load rbenv
  if [[ -d "${HOME}/.rbenv" ]]; then
    export PATH="${HOME}/.rbenv/bin:$PATH"
    eval "$(rbenv init --no-rehash -)"
  fi

  # Load pyenv
  if [[ -d "${HOME}/.pyenv" ]]; then
    export PATH="${HOME}/.pyenv/bin:$PATH"
    eval "$(pyenv init --no-rehash -)"
  fi

  # Load GVM
  #if [[ -d "${HOME}/.gvm" ]]; then
  #  source "${HOME}/.gvm/scripts/gvm"
  #fi

  # Color grep results
  export GREP_OPTIONS='--color=auto'
  export GREP_COLOR='1;32'
fi
