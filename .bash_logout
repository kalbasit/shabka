case "`tty`" in
	/dev/vc/[0-9]) clear
esac

rm -rf ~/.serverauth.*

if [[ -x /usr/bin/sudo ]]; then
    sudo -k
fi
