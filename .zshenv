# if we are logging in from the tty1 on the console, start X immediately
if [[ "$(tty)" = "/dev/tty1" ]]; then
	if [[ -f "${HOME}/.xsession-errors" ]]; then
		rm -f "${HOME}/.xsession-errors.old"
		mv "${HOME}/.xsession-errors" "${HOME}/.xsession-errors.old"
	fi
	exec startx &> "${HOME}/.xsession-errors"
fi
