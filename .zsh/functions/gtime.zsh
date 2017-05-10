#
# vim:ft=zsh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#

function gtime() {
  local branch=""

  if [[ "x$(git rev-parse --git-dir 2> /dev/null)" != "x" ]]; then
    branch="$(current_branch)"
  else
    echo "You must be under a git repository to use gcim"
    return 1
  fi

  git reflog --date=relative --all | grep "refs/heads/${branch}@.*: branch: Created" | sed -e 's:.*{\([^}]*\)}.*:\1:g'
}
