function git_gopath_formatted_repo_path() {
  git remote get-url origin | \
    sed -e 's#^\(http://\|https://\|git@\)\(.*\)\(\.git\)$#\2#g' | \
    tr ':' '/'
}
