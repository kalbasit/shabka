#
# vim:ft=zsh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

# ne() {{{
function ne() {
    # get the name from the arguments
    local name="${1}"
    if [[ "x${name}" = "x" ]]; then
        print_error 0 "you must provide a name"
        return 1
    fi
    # create the directory
    mkdir -p "${GOPATH}/src/experimental/${name}"
    # TODO: should I switch profile off? maybe have an argument for it?
    tmx "${GOPATH}/src/experimental/${name}"
    return 0
}
# }}}
