# Make pathmunge is defined
if ! type pathmunge &> /dev/null; then
  function pathmunge() {
      if ! [[ $PATH =~ (^|:)$1($|:) ]]; then
         if [ "$2" = "after" ] ; then
            PATH=$PATH:$1
         else
            PATH=$1:$PATH
         fi
      fi
  }
fi

if [[ -d /usr/local/go/bin ]]; then
  pathmunge /usr/local/go/bin
fi

export EDITOR=vim
pathmunge "${HOME}/.bin"

if [[ -d "${HOME}/.filesystem" ]]; then
  export MYFS="${HOME}/.filesystem"
  pathmunge "${MYFS}/bin"

  if [[ -d "${MYFS}/opt" ]]; then
    if [[ -d "${MYFS}/opt/go_appengine" ]]; then
      pathmunge "${MYFS}/opt/go_appengine"
    fi

    for dir in `ls --color=never "${MYFS}/opt"`; do
      if [[ -d "${MYFS}/opt/${dir}/bin" ]]; then
        pathmunge "${MYFS}/opt/${dir}/bin"
      fi
    done
  fi

  # Make LD can find our files.
  export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${HOME}/.filesystem/lib"
fi

export GOPATH="${HOME}/code"
pathmunge "${GOPATH}/bin"

if [[ -d "${HOME}/.cask/bin" ]]; then
  pathmunge "${HOME}/.cask/bin"
  export CASK_PATH="$(dirname $(dirname $(which cask)))"
fi

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

# Color grep results
export GREP_OPTIONS='--color=auto'
export GREP_COLOR='1;32'
