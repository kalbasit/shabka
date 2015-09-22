export GOPATH="${HOME}/code"        # How can I live without Go?
export GO15VENDOREXPERIMENT=1       # https://github.com/golang/go/commit/183cc0cd41f06f83cb7a2490a499e3f9101befff
export MYFS="${HOME}/.filesystem"   # Make sure I always know about my filesystem.
export EDITOR=vim                   # Vim Vim Vim baby!!
export GREP_OPTIONS='--color=auto'  # grep always try to use colors.
export GREP_COLOR='1;32'            # define grep's color.

function pathmunge() {
  [[ ! -d "${1}" ]] && return
  if ! [[ $PATH =~ (^|:)$1($|:) ]]; then
     if [ "$2" = "after" ] ; then
        PATH=$PATH:$1
     else
        PATH=$1:$PATH
     fi
  fi
}

function pathunmunge() {
  [[ ! -d "${1}" ]] && return
  oldpath=("${(@s/:/)PATH}")
  newpath=""
  sep=""
  for p in ${oldpath[*]}; do
    if [[ "x${p}" != "x${1}" ]]; then
      newpath="${newpath}${sep}${p}"
      sep=":"
    fi
  done
  export PATH="${newpath}"
}

# Are we on Mac? Start the path as defined in /etc/paths
if [[ -x /usr/libexec/path_helper ]]; then
  eval `/usr/libexec/path_helper -s`
fi

# We need our bin folder
pathmunge "${HOME}/.bin"

# We need Go's bin folder
pathmunge "${GOPATH}/bin"

# Anything got installed into MYFS?
pathmunge "${MYFS}/bin"
pathmunge "${MYFS}/opt/go_appengine"
if [[ -d "${MYFS}" ]]; then
  if [[ -d "${MYFS}/opt" ]]; then
    for dir in `find "${MYFS}/opt" -maxdepth 1 -mindepth 1 -type d`; do
      if [[ -d "${dir}/bin" ]]; then
        pathmunge "${dir}/bin" after
      fi
    done
  fi

  # Make LD can find our files.
  export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${HOME}/.filesystem/lib"
fi

pathmunge "${GOPATH}/src/github.com/phacility/arcanist/bin"
pathmunge "${GOPATH}/src/github.com/google/bazel/output"

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

# Load travis
if [[ -f "${HOME}/.travis/travis.sh" ]]; then
  source "${HOME}/.travis/travis.sh"
fi

# Export Github's token if it's readable.
github_token_path="$HOME/.github_token"
if [[ -r "${github_token_path}" ]]; then
  export HOMEBREW_GITHUB_API_TOKEN=`head -1 ${github_token_path}`
fi
unset github_token_path

# Export MySQL credentials if it's readable
mysql_credentials_path="$HOME/.my.cnf"
if [[ -r "${mysql_credentials_path}" ]]; then
  user="`cat "$mysql_credentials_path" | grep "user" | cut -d= -f2 | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'`"
  pass="`cat "$mysql_credentials_path" | grep "password" | cut -d= -f2 | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'`"

  if [ "x${user}" != "x" ]; then
    export MYSQL_USERNAME="${user}"
    export MYSQL_PASSWORD="${pass}"
  fi
  unset user pass
fi
unset mysql_credentials_path
