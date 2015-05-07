function gcim() {
  local message="${@}"
  local project=""
  local branch=""
  local story=""

  if [[ "x`git rev-parse --git-dir 2> /dev/null`" != "x" ]]; then
    branch="`current_branch`"
    story="`echo "${branch}" | grep '^\(DAILY\|DMX\)-[[:digit:]][[:digit:]]*$'`"
  else
    echo "You must be under a git repository to use gcim"
    return 1
  fi

  if [[ -z "${message}" ]]; then
    echo "USAGE: gcim <message>"
    return 1
  fi

  if echo "${PWD}" | grep -q "/dmx\|dailymotion/go-liverail"; then
    project="ADS"
  fi

  if [[ -n "${PROJECT}" ]]; then
    project="${PROJECT}"
  fi

  if [[ -z "${project}" ]]; then
    project="`echo "${branch}" | grep '^[Uu][Ss][0-9]*[-_]' \
      | sed -e 's:^\([Uu][Ss][0-9]*\)[-_].*:\1:g' \
      | tr 'a-z' 'A-Z'`"
  fi

  if [[ -z "${project}" ]]; then
    echo "Missing project, please export the project name as PROJECT and amend the commit if needed"
  else
    project="[${project}] "
  fi


  if [[ "x${story}" != "x" ]]; then
    git commit -m "${project}${message} (${story})"
  else
    git commit -m "${project}${message}"
  fi
}
