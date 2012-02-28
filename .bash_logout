
# ~/.bash_logout: executed by bash(1) when login shell exits.

# when leaving the console clear the screen to increase privacy
if [ "$SHLVL" = 1 ]; then
  [ -x /usr/bin/clear_console ] && /usr/bin/clear_console -q
fi
case "`tty`" in
  /dev/vc/[0-9]) clear
esac

# Remove .serverauth files
rm -rf ~/.serverauth.*

# Logout from sudo
if [[ -x /usr/bin/sudo ]]; then
  sudo -k
fi
