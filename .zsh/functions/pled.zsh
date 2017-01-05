#
# vim:ft=zsh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

# pled() {{{
function pled() {
    plutil -convert xml1 ${1}
    ${EDITOR} ${1}
    plutil -convert binary1 ${1}
}
# }}}
