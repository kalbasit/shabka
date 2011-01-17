case "`tty`" in
	/dev/vc/[0-9]) clear
esac

if which sudo &> /dev/null; then
    sudo -k
fi
