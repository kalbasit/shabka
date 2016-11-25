#
# vim:ft=sh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

# sp() #{{{
function sp() {
    profiles=()
    for wpf in ${HOME}/.zsh/profiles/*.zsh; do
        f="`basename "${wpf}"`"
        profiles=(${profiles[@]} ${f%%.zsh})
    done
    if [ "${#}" -eq "0" -o "${1}" = "ls" ]; then
        for wpf in ${profiles[@]}; do
            if [[ "x${wpf}" = "x${ACTIVE_PROFILE}" ]]; then
                echo "${FG_GREEN}*${FG_CLEAR} ${wpf}"
            else
                echo "  ${wpf}"
            fi
        done
    elif [[ "${1}" = "kill" ]]; then
        if [[ -n "${ACTIVE_PROFILE}" ]]; then
            source "${HOME}/.zsh/profiles/${ACTIVE_PROFILE}.zsh"
            pdeactivate
            unset ACTIVE_PROFILE SSH_AGENT_PID SSH_AUTH_SOCK SSH_AGENT_NAME
            eval `ssh-agents $SHELL`
        fi
    else
        if [[ ! -e "${HOME}/.zsh/profiles/${1}.zsh" ]]; then
            echo "profile ${1} not found."
            return 1
        fi

        sp kill
        source "${HOME}/.zsh/profiles/${1}.zsh"
        pactivate
        export ACTIVE_PROFILE="${1}"
        export SSH_AGENT_NAME="${1}"
        unset SSH_AGENT_PID SSH_AUTH_SOCK
        eval `ssh-agents $SHELL`
    fi
}
#}}}
