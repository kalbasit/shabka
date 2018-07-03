# if we are logging in from the tty1 on the console, start Sway immediately
if [[ "$(tty)" = "/dev/tty1" ]]; then
	exec .libexec/sway/sway-run
fi
