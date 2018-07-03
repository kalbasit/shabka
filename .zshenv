# if we are logging in from the tty1 on the console, start X immediately
if [[ "$(tty)" = "/dev/tty1" ]]; then
	if [[ -f "${HOME}/.xsession-errors" ]]; then
		rm -f "${HOME}/.xsession-errors.old"
		mv "${HOME}/.xsession-errors" "${HOME}/.xsession-errors.old"
	fi

	export XKB_DEFAULT_LAYOUT=us
	export XKB_DEFAULT_VARIANT=colemak
	export XKB_DEFAULT_OPTIONS=ctrl:nocaps

	exec sway &> "${HOME}/.xsession-errors"
fi
