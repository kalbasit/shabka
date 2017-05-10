#
# vim:ft=zsh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#

function gorder() {
  local branch

  for branch in $(git branch | sed s/^..//); do
    echo -e "$(git log -1 --pretty=format:"%Cgreen%ci %Cblue%cr%Creset" "${branch}")\t${branch}"
  done | sort -r
}

