# sw stands for Start Working and it basically ensures that all of the projects
# that I need are open in a tmux session
function sw() {
    local current_profile=
    local dir=
    local i=
    local profile=
    local rc="${HOME}/.swrc"
    local rc_length=

    # compute the length of the rc file
    rc_length="$( jq length "${rc}" )"
    # record the current profile
    current_profile="${ACTIVE_PROFILE}"
    for (( i = 0; i < rc_length; i++)); do
        profile="$( jq -r ".[${i}] .profile" "${rc}" )"
        # TODO(kalbasit): I cannot figure out how to interpolate variables
        # stored in the dir variable in rc. i.e it's always
        # $GOPATH/src/github.com/... and I cannot turn $GOPATH to the value
        dir="$( jq -r ".[${i}] .dir" "${rc}" | sed -e "s:\$GOPATH:$GOPATH:g" -e "s:\$HOME:$HOME:g" )"
        if [[ -z "${profile}" ]]; then
            print_error 0 "profile cannot be empty"
            print_error 1 "$(jq -r .[${i}] "${rc}" )"
        fi
        print_info 0 "starting ${dir} under the ${profile} profile"
        sp "${profile}" 2
        tmx --log-depth 2 --ignore-if-session-exists --new --start-detached "${dir}"
    done
    # restore the current profile
    if [[ -n "${current_profile}" ]]; then
        sp "${current_profile}"
    fi
    # open the dotfiles tmux
    tmx "${HOME}/.dotfiles"
}
