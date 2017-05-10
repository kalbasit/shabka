#
# vim:ft=zsh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#

# github_commit_link [REF]
#
# Print the Github commit link for REF (default HEAD)
github_commit_link() { # [REF]
  # get the reference we need
  local ref="${1:-HEAD}"
  # compute the path to the root of the repository
  local repo_dir="$(git rev-parse --show-toplevel)"
  # validate we are under a Github repo
  # TODO: must check the URL of the origin if this failed for repos outside
  # $GOPATH
  if ! echo "${repo_dir}" | grep -q -i '/github\.com/[^/]*/[^/]*'; then
    print_error 0 "this only works for Github.com"
    return 1
  fi
  # compute the user from the path
  local user="$(basename "$(dirname "${repo_dir}")")"
  # compute the repo from the path
  local repo="$(basename "${repo_dir}")"
  # get the commit from the ref
  local commit="$(git show --no-patch --format="%H" "${ref}")"
  # finally echo it
  echo "https://github.com/${user}/${repo}/commit/${commit}"
}
