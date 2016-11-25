#
# vim:ft=sh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

# xmlpp() #{{{
function xmlpp() {
    if [[ "${#}" -eq 0 ]]; then
        xmllint --format -
    else
        input_file="${1}"
        if [[ "${#}" -eq 2 ]]; then
            output_file="${2}"
        else
            output_file="`mktemp /tmp/xmlpp.XXXXXXXX`"
        fi

        xmllint --format --output "${output_file}" "${input_file}" || return
        mv "${output_file}" "${input_file}"
    fi
}
#}}}
