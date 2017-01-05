#
# vim:ft=zsh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

# inArray()#{{{
function inArray()
{
    local i
    [[ "${#}" -lt "2" ]] && return 1
    needle="${1}"
    shift
    haystack=(${@})
    for i in "${haystack[@]}"; do
        [[ "${needle}" = "${i}" ]] && return 0
    done
    return 1
}
#}}}
