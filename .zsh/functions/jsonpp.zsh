#
# vim:ft=sh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

# jsonpp() #{{{
function jsonpp() {
    if [[ "${#}" -eq 0 ]]; then
        python -m json.tool | pygmentize -l javascript
    else
        input_file="${1}"
        if [[ "${#}" -eq 2 ]]; then
            output_file="${2}"
        else
            output_file="`mktemp /tmp/xmlpp.XXXXXXXX`"
        fi

        python -m json.tool < "${input_file}" > "${output_file}" || return
        mv "${output_file}" "${input_file}"
    fi
}
#}}}
