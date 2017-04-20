function clone() { # URL
  local url="${1}"
  # compute the path of the clone
  local clone_path="${GOPATH}/src/$(echo "${url}" | sed -e 's/^\(http:\/\/\|https:\/\/\|git@\)\(.*\)\(\.git\)$/\2/g' | tr ':' '/')"
  # make sure we do not clone an already cloned repo
  if [[ -d "${clone_path}" ]]; then
    print_error "${url} is already cloned at ${clone_path}"
    return 1
  fi
  # make sure the directory is avalable
  mkdir -p "$(dirname "${clone_path}")"
  # clone it now
  git clone "${url}" "${clone_path}"
}

