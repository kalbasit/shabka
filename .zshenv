# if we are logging in from the tty1 on the console, start X immediately
if [[ "$(tty)" = "/dev/tty1" ]]; then
	mv ~/.xsession-errors ~/.xsession-errors.old
	exec startx > ~/.xsession-errors
fi
