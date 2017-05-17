function git_gopath_formatted_repo_path() {
  repo_url="${1}"
  [[ -z "${repo_url}" ]] && repo_url="$( git remote get-url origin )"
  echo "${repo_url}" | \
    sed -e 's#^\(http://\|https://\|git@\)\(.*\)\(\.git\)$#\2#g' | \
    tr ':' '/'
}
