#
# vim:ft=sh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

# mkd() {{{
# Taken from https://github.com/mathiasbynens/dotfiles/blob/master/.functions
function mkd() {
    mkdir -p "$@" && cd "$_";
}
# }}}
