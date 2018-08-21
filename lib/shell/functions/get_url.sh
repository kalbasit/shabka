function get_url() {
	grep '"url":' "${1}" | cut -d'"' -f 4 | cut -d. -f -2
}

