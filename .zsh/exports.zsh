export GOPATH="${HOME}/code"        # How can I live without Go?
export MYFS="${HOME}/.filesystem"   # Make sure I always know about my filesystem.
export HOMEBREW_CASK_OPTS="--appdir=/Applications"
export EDITOR=vim                   # Vim Vim Vim baby!!
export GREP_OPTIONS='--color=auto'  # grep always try to use colors.
export GREP_COLOR='1;32'            # define grep's color.
export BROWSER=/usr/bin/google-chrome-unstable

# Make sure pathmunge is defined
if ! type pathmunge > /dev/null 2>&1; then
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

# Are we on Mac? Start the path as defined in /etc/paths
if [[ -x /usr/libexec/path_helper ]]; then
  eval `/usr/libexec/path_helper -s`
fi

# We need our bin folder
pathmunge "${HOME}/.bin"

# We need Go's bin folder
pathmunge "${GOPATH}/bin"

# Anything got installed into MYFS?
if [[ -d "${MYFS}" ]]; then
  pathmunge "${MYFS}/bin"

  if [[ -d "${MYFS}/opt" ]]; then
    if [[ -d "${MYFS}/opt/go_appengine" ]]; then
      pathmunge "${MYFS}/opt/go_appengine"
    fi

    for dir in `find "${MYFS}/opt" -maxdepth 1 -mindepth 1 -type d`; do
      if [[ -d "${dir}/bin" ]]; then
        pathmunge "${dir}/bin"
      fi
    done
  fi

  # Make LD can find our files.
  export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${HOME}/.filesystem/lib"
fi

# Are we using emacs's cask?
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


# Export Github's token if it's readable.
local github_token_path="$HOME/.github_token"
if [[ -r "${github_token_path}" ]]; then
  export HOMEBREW_GITHUB_API_TOKEN=`head -1 ${github_token_path}`
fi

# Export MySQL credentials if it's readable
local mysql_credentials_path="$HOME/.my.cnf"
if [[ -r "${mysql_credentials_path}" ]]; then
  local user="`cat "$mysql_credentials_path" | grep "user" | cut -d= -f2 | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'`"
  local pass="`cat "$mysql_credentials_path" | grep "password" | cut -d= -f2 | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'`"

  if [ "x${user}" != "x" ]; then
    export MYSQL_USERNAME="${user}"
    export MYSQL_PASSWORD="${pass}"
  fi
fi
