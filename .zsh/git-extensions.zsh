function gcim() {
  local message="${@}"
  local project=""

  if [[ -z "${message}" ]]; then
    echo "USAGE: gcim <message>"
    return 1
  fi

  if echo "${PWD}" | grep -q "ads/dmx"; then
    project="DMX"
  fi

  if [[ -z "${project}" ]]; then
    if [[ -z "${DM_PROJECT}" ]]; then
      echo "Missing project, please export the project name as DM_PROJECT"
      return 1
    fi

    project="${DM_PROJECT}"
  fi

  if [[ "x`git rev-parse --git-dir 2> /dev/null`" != "x" ]]; then
    local branch="`current_branch`"
    local story="`echo "${branch}" | grep '^\(DAILY\|DMX\)-[[:digit:]][[:digit:]]*$'`"
    if [[ "x${story}" != "x" ]]; then
      git commit -m "[${project}] ${message} (${story})"
    else
      git commit -m "${message}"
    fi
  else
    echo "You must be under a git repository to use gcim"
    return 1
  fi
}
