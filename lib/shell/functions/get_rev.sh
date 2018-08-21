function get_rev() {
	grep '"rev":' "${1}" | cut -d'"' -f 4
}
