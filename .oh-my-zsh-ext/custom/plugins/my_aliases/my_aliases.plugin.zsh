# Which
function _which_()
{
  which_path=`/usr/bin/env which -a which 2>/dev/null | grep '^/'`
  $which_path $@ 2> /dev/null
}

# Make grepping for a running process as each as one call
alias pw='ps aux | grep -v grep | grep -e'

# tail -f alias
alias tf='tail -f'

# Don't let VІM launch in compatible mode.
if which vim &> /dev/null; then
    alias vi='vim'
fi

# Stuff that use sudo.
if [ -x "$(which sudo 2> /dev/null)" ]; then
    alias shutdown='sudo shutdown'
    alias reboot='sudo reboot'
    alias poweroff='sudo poweroff'
    alias svi='sudo vim'
    alias svim='sudo vim'
    alias mount='sudo mount'
    alias umount='sudo umount'
    if [ "${SYSTEM}" = "Darwin" ]; then
      alias sudosu="sudo su -m root -c $(dscl . -read /Users/wael UserShell | cut -d' ' -f 2)"
    else
      alias sudosu="sudo su -c \"HOME=${HOME} $(grep ${USER} /etc/passwd | cut -d':' -f7 )\""
    fi
fi

# Common mistakes
alias xs='cd'
alias vf='cd'
alias ms='ls'
alias vu='vi'
alias sl='ls'
alias gti='git'

# Aria2 Aliases
if [ -f "${HOME}/Library/Application Support/Google/Chrome/Default/Cookies" ]; then
  ARIA2_COOKIE_FILE="${HOME}/Library/Application Support/Google/Chrome/Default/Cookies"
elif [ -f "${HOME}/.mozilla/firefox/eMxyzptlk/cookies.sqlite" ]; then
  ARIA2_COOKIE_FILE="${HOME}/.mozilla/firefox/eMxyzptlk/cookies.sqlite"
fi
if [ -n "${ARIA2_COOKIE_FILE}" ]; then
  alias aria2c="dumpcookies -i '${ARIA2_COOKIE_FILE}' -o '/tmp/cookies.txt' &> /dev/null; chmod 600 '/tmp/cookies.txt' &> /dev/null; aria2c --file-allocation=none --load-cookies='/tmp/cookies.txt' --ftp-pasv -c -m 3"
  unset ARIA2_COOKIE_FILE
fi

# Mysql
if [ -x "`_which_ mysql`" ]; then
  alias myc='mysql --default-character-set=utf8'
  alias myd='mysqldump --default-character-set=utf8'
fi

# ViM
if [ -x "`_which_ gvim`" ]; then
  alias e='gvim --remote-silent'
fi
if [ -x "`_which_ mvim`" ]; then
  alias e='mvim --remote-silent'
fi

# Xrandr
alias xrandron-work='xrandr --output LVDS1 --mode 1366x768 --noprimary --output HDMI1 --mode 1920x1080 --right-of LVDS1 --primary'
alias xrandron-home='xrandr --output LVDS1 --mode 1366x768 --noprimary --output VGA1 --mode 1680x1050 --right-of LVDS1 --primary'
alias xrandroff='xrandr --output LVDS1 --mode 1366x768 --output VGA1 --off --output HDMI1 --off'
alias xrandroff-lvds='xrandr --output LVDS1 --off'
