#
# vim:ft=sh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

function mkmine() {
    local user="$( id -u )"
    local group="$( id -g )"
    local i=
    local flagsdone=false
    local args=()

    # should we recursively chown?
    for i in {0..${#}}; do
        if [[ "${1}" =~ ^- ]]; then
            if isTrue "{flagsdone}"; then
                # the user entered FLAG* FILE FLAG*
                print_error 0 "USAGE: mkmine [FLAG,...] <FILE,...>"
                return 1
            fi
            args=("${args[@]}" ${1})
            shift
        else
            flagsdone=true
        fi
    done

    debug "args=(${args[@]})"
    debug "user=${user}"
    debug "group=${group}"
    debug "\${@} = ${@}"

    # chown the requested paths
    debug sudo chown ${args[@]} "${user}:${group}" "${@}"
    sudo chown "${args[@]}" "${user}:${group}" "${@}"
}
