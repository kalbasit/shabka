# if we are logging in from the tty1 on the console, start X immediately
if [[ "$(tty)" = "/dev/tty1" ]]; then
	rm -f "${HOME}/.xsession-errors.old"
	mv "${HOME}/.xsession-errors" "${HOME}/.xsession-errors.old"
	exec startx &> "${HOME}/.xsession-errors"
fi
