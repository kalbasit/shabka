#
# vim:ft=zsh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

# isTrue()#{{{
function isTrue() {
    case "${1}" in
        [Tt][Rr][Uu][Ee])
            return 0
            ;;
        [Tt])
            return 0
            ;;
        [Yy][Ee][Ss])
            return 0
            ;;
        [Yy])
            return 0
            ;;
        1)
            return 0
            ;;
        *)
            return 1
            ;;
    esac
}
#}}}
