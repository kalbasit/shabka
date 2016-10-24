function gcim() {
  local message="${@}"
  local project=""
  local branch=""
  local story=""

  if [[ "x$(git rev-parse --git-dir 2> /dev/null)" != "x" ]]; then
    branch="$(current_branch)"
    story="$(echo "${branch}" | grep '^[A-Z][A-Z]*-[[:digit:]][[:digit:]]*$')"
  else
    echo "You must be under a git repository to use gcim"
    return 1
  fi

  if [[ -z "${message}" ]]; then
    echo "USAGE: gcim <message>"
    return 1
  fi

  if [[ "x${story}" != "x" ]]; then
    git commit -m "${message} (${story})"
  else
    git commit -m "${message}"
  fi
}

function gtime() {
  local branch=""

  if [[ "x$(git rev-parse --git-dir 2> /dev/null)" != "x" ]]; then
    branch="$(current_branch)"
  else
    echo "You must be under a git repository to use gcim"
    return 1
  fi

  git show --pretty=format:"%Cblue%cr%Creset" "${branch}" | head -1
}
