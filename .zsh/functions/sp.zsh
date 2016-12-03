# sp stands for switch profile and it is designed to change the work profile of
# the terminal by calling the appropriate pactivate() and pdeactivate() on the
# profile
function sp() {
    local profiles=()
    local requested_profile="${1}"
    local log_depth="${2}"
    local pf=

    # sanity check
    if ! [[ "${log_depth}" -ge 0 ]]; then
        log_depth=0
    fi
    # load the available profiles
    for pf in ${HOME}/.zsh/profiles/*.zsh; do
        f="$(basename "${pf}")"
        profiles=(${profiles[@]} ${f%%.zsh})
    done
    # are we only trying to list the available profiles?
    if [[ "${#}" -eq "0" ]] || [[ "${requested_profile}" = "ls" ]]; then
        for pf in "${profiles[@]}"; do
            if [[ "x${pf}" = "x${ACTIVE_PROFILE}" ]]; then
                echo -e "$(for i in $(seq 1 ${log_depth}); do echo -ne " "; done)${FG_GREEN}*${FG_CLEAR} ${pf}"
            else
                echo -e "$(for i in $(seq 1 ${log_depth}); do echo -ne " "; done)  ${pf}"
            fi
        done
    # are we loading the personal profile (i.e removing profiles?)
    elif [[ "${requested_profile}" = "kill" ]] || [[ "${requested_profile}" = "personal" ]]; then
        if [[ -n "${ACTIVE_PROFILE}" ]]; then
            # shellcheck disable=SC1090
            source "${HOME}/.zsh/profiles/${ACTIVE_PROFILE}.zsh"
            pdeactivate
            unset ACTIVE_PROFILE SSH_AGENT_PID SSH_AUTH_SOCK SSH_AGENT_NAME
            eval "$(ssh-agents "$SHELL")"
        fi
    # are we actually loading a profile?
    else
        if [[ ! -e "${HOME}/.zsh/profiles/${requested_profile}.zsh" ]]; then
            print_error "${log_depth}" "profile ${HOME}/.zsh/profiles/${requested_profile}.zsh not found"
            return 1
        fi

        # unload the current profile
        sp kill
        # activate the profile
        # shellcheck disable=SC1090
        source "${HOME}/.zsh/profiles/${requested_profile}.zsh"
        pactivate
        # let the terminal know whick profile is loaded and which ssh-agent to
        # use
        export ACTIVE_PROFILE="${requested_profile}"
        export SSH_AGENT_NAME="${requested_profile}"
        unset SSH_AGENT_PID SSH_AUTH_SOCK
        eval "$(ssh-agents "$SHELL")"
    fi
}
#}}}
