#####################################################################
# core
#####################################################################

# Define the location of the .zsh folder
ZSH="${ZDOTDIR:-$HOME}/.zsh"

# Set ZSH_CACHE_DIR to the path where cache files should be created
# or else we will use the default cache/
if [[ -z "$ZSH_CACHE_DIR" ]]; then
  ZSH_CACHE_DIR="$ZSH/cache"
fi

# Figure out the SHORT hostname
if [[ -n "$commands[scutil]" ]]; then
  # OS X
  SHORT_HOST=$(scutil --get ComputerName)
else
  SHORT_HOST=${HOST/.*/}
fi

# Save the location of the current completion dump file.
ZSH_COMPDUMP="${ZDOTDIR:-$HOME}/.zcompdump-${SHORT_HOST}-${ZSH_VERSION}"

# Load all stock functions (from $fpath files) called below.
autoload -U compaudit compinit
# If completion insecurities exist, warn the user without enabling completions.
if ! compaudit &>/dev/null; then
  # This function resides in the "lib/compfix.zsh" script sourced above.
  handle_completion_insecurities
  # Else, enable and cache completions to the desired file.
else
  compinit -d "${ZSH_COMPDUMP}"
fi

#####################################################################
# zplug
#####################################################################

# if zplug is not installed, do it
if [[ ! -f "${HOME}/.zplug/init.zsh" ]]; then
  if [[ -d "${HOME}/.zplug" ]]; then
    echo "FATAL: ${HOME}/.zplug is present but the init.zsh is not"
    return 1
  fi
  git clone https://github.com/zplug/zplug.git "${HOME}/.zplug"
fi

# Load zplug
source "${HOME}/.zplug/init.zsh"
# let zplug manage itself
zplug 'zplug/zplug', hook-build:'zplug --self-manage'

# load the shellder theme
zplug "simnalamburt/shellder", as:theme

# plugins
zplug "b4b4r07/emoji-cli"
zplug "hcgraf/zsh-sudo"
zplug "plugins/command-not-found", from:oh-my-zsh
zplug "plugins/extract",           from:oh-my-zsh
zplug "plugins/git",               from:oh-my-zsh
zplug "plugins/github",            from:oh-my-zsh
zplug "plugins/history",           from:oh-my-zsh
zplug "plugins/zsh_reload",        from:oh-my-zsh
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-syntax-highlighting"

# Install any missing zplug plugin
zplug check || zplug install

# Then, source plugins and add commands to $PATH
zplug load

#####################################################################
# functions
#####################################################################

for func in ${ZSH}/functions/*.zsh; do
    # shellcheck disable=SC1090
    source "${func}"
done

#####################################################################
# aliases
#####################################################################

# Conditional aliases
[[ -x "`which ack-grep 2> /dev/null`" ]] && alias ack="ack-grep -il"
[[ -x "`which nvim 2> /dev/null`" ]] && alias vim="nvim"

# Aliases
alias vi=vim
alias e="${EDITOR:-vim}"
alias pw="ps aux | grep -v grep | grep -e"
alias serve_this="python2 -m SimpleHTTPServer"
alias rserve_this="ruby -rrack -e \"Rack::Handler::WEBrick.run Rack::Directory.new('.')\""
alias history='fc -fl 1'
alias comp=docker-compose
alias mach=docker-machine
alias http='http --print=HhBb'
alias blaze=bazel
alias rot13="tr '[A-Za-z]' '[N-ZA-Mn-za-m]'"
alias kp='sp kill'
alias dmx='sp dmx'
alias tt='sp talentoday'
# Always enable colored `grep` output
# Note: `GREP_OPTIONS="--color=auto"` is deprecated, hence the alias usage.
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias remove_dead_containers="docker rm -v \$(docker ps -a -q -f status=exited)"
alias remove_created_containers="docker rm -v \$(docker ps -a -q -f status=created)"
alias remove_dangling_images="docker rmi \$(docker images -f "dangling=true" -q)"
alias irc='tmux attach -t irc || tmux new -s irc irssi'
alias gl='github_commit_link'
alias utf8test='curl -L https://github.com/tmux/tmux/raw/master/tools/UTF-8-demo.txt'

# General aliases
alias -g rocker_auth="--auth kalbasit:\$(lpass show --password 4984935876)"

# Mac only
if [[ "$(uname)" = "Darwin" ]]; then
  alias mac_install_cert='sudo security add-trusted-cert -d -r trustRoot -k /Library/Keychains/System.keychain'
  alias upup='brew upgrade'
fi

# Linux only
if [[ "$(uname)" = "Linux" ]]; then
  alias upup='yaourt -Syu --aur'
fi

#####################################################################
# colors
#####################################################################

# Defining some color
export FG_CLEAR="\033[0m"

# Regular ForeGround colors
export FG_BLACK="\033[0;30m"
export FG_RED="\033[0;31m"
export FG_GREEN="\033[0;32m"
export FG_YELLOW="\033[0;33m"
export FG_BLUE="\033[0;34m"
export FG_MAGNETA="\033[0;35m"
export FG_CYAN="\033[0;36m"
export FG_WHITE="\033[0;37m"

# Bold ForeGround colors
export FG_BLACK_B="\033[1;30m"
export FG_RED_B="\033[1;31m"
export FG_GREEN_B="\033[1;32m"
export FG_YELLOW_B="\033[1;33m"
export FG_BLUE_B="\033[1;34m"
export FG_MAGNETA_B="\033[1;35m"
export FG_CYAN_B="\033[1;36m"
export FG_WHITE_B="\033[1;37m"

# Background colors
export BG_BLACK="\033[40m"
export BG_RED="\033[41m"
export BG_GREEN="\033[42m"
export BG_YELLOW="\033[43m"
export BG_BLUE="\033[44m"
export BG_MAGNETA="\033[45m"
export BG_CYAN="\033[46m"
export BG_WHITE="\033[47m"

# GOOD, WARN and ERROR colors
export GOOD="${FG_GREEN_B}"
export WARN="${FG_YELLOW_B}"
export ERROR="${FG_RED_B}"

# Enable ls colors
if [[ "$DISABLE_LS_COLORS" != "true" ]]; then
  autoload colors; colors;
  export LSCOLORS="Gxfxcxdxbxegedabagacad"

  # Find the option for using colors in ls, depending on the version: Linux or BSD
  if [[ "$(uname -s)" == "NetBSD" ]]; then
    # On NetBSD, test if "gls" (GNU ls) is installed (this one supports colors); 
    # otherwise, leave ls as is, because NetBSD's ls doesn't support -G
    gls --color -d . &>/dev/null 2>&1 && alias ls='gls --color=tty'
  elif [[ "$(uname -s)" == "OpenBSD" ]]; then
    # On OpenBSD, test if "colorls" is installed (this one supports colors);
    # otherwise, leave ls as is, because OpenBSD's ls doesn't support -G
    colorls -G -d . &>/dev/null 2>&1 && alias ls='colorls -G'
  else
    ls --color -d . &>/dev/null 2>&1 && alias ls='ls --color=tty' || alias ls='ls -G'
  fi
fi

#####################################################################
# completion
#####################################################################

# fixme - the load process here seems a bit bizarre

unsetopt menu_complete   # do not autoselect the first completion entry
unsetopt flowcontrol
setopt auto_menu         # show completion menu on succesive tab press
setopt complete_in_word
setopt always_to_end

WORDCHARS=''

zmodload -i zsh/complist

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

zstyle ':completion:*' list-colors ''

# should this be in keybindings?
bindkey -M menuselect '^o' accept-and-infer-next-history

zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u `whoami` -o pid,user,comm -w -w"

# disable named-directories autocompletion
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
cdpath=(.)

# Use caching so that commands like apt and dpkg complete are useable
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path $ZSH/cache/

# Don't complete uninteresting users
zstyle ':completion:*:*:*:users' ignored-patterns \
        adm amanda apache at avahi avahi-autoipd beaglidx bin cacti canna \
        clamav daemon dbus distcache dnsmasq dovecot fax ftp games gdm \
        gkrellmd gopher hacluster haldaemon halt hsqldb ident junkbust kdm \
        ldap lp mail mailman mailnull man messagebus  mldonkey mysql nagios \
        named netdump news nfsnobody nobody nscd ntp nut nx obsrun openvpn \
        operator pcap polkitd postfix postgres privoxy pulse pvm quagga radvd \
        rpc rpcuser rpm rtkit scard shutdown squid sshd statd svn sync tftp \
        usbmux uucp vcsa wwwrun xfs

# ... unless we really want to.
zstyle '*' single-ignored show

# Show dots while doing completion
expand-or-complete-with-dots() {
  echo -n "\e[31m......\e[0m"
  zle expand-or-complete
  zle redisplay
}
zle -N expand-or-complete-with-dots
bindkey "^I" expand-or-complete-with-dots

[[ -r "/usr/local/share/zsh/site-functions/_aws" ]] && source "/usr/local/share/zsh/site-functions/_aws"

#####################################################################
# directories
#####################################################################

# Changing/making/removing directory
setopt auto_name_dirs
setopt pushd_ignore_dups
setopt pushdminus
setopt autopushd
setopt pushdsilent
setopt pushdtohome
setopt auto_cd
setopt multios
setopt cdablevarS

alias ..='cd ..'
alias cd..='cd ..'
alias cd...='cd ../..'
alias cd....='cd ../../..'
alias cd.....='cd ../../../..'
alias cd/='cd /'

alias 1='cd -'
alias 2='cd -2'
alias 3='cd -3'
alias 4='cd -4'
alias 5='cd -5'
alias 6='cd -6'
alias 7='cd -7'
alias 8='cd -8'
alias 9='cd -9'

alias md='mkdir -p'
alias rd=rmdir
alias d='dirs -v | head -10'

#####################################################################
# exports
#####################################################################

export BROWSER="${HOME}/.bin/relay-browser"           # Set the browser to my relay browser
export GPG_TTY="$(tty)"                               # GPG_TTY is needed for gpg with pinentry-curses
export GLOBAL_GOPATH="${HOME}/code"
export GOPATH="${GLOBAL_GOPATH}"
export CDPATH="${GOPATH}/src:$cdpath"                 # Add $GOPATH/src to CDPATH
export MYFS="${HOME}/.filesystem"                     # Make sure I always know about my filesystem.
export EDITOR="$(which nvim)"                         # NeoVim simply rocks!!
export SUDO_EDITOR="$(which nvim)"                    # https://wiki.archlinux.org/index.php/security#Editing_files_using_sudo
export NOTMUCH_CONFIG="${HOME}/.mail/.notmuch/config" # the path to notmuch config file
export PAGER="less"
export LANG=en_US.UTF-8
export LC_ALL="${LANG}"
[[ -n "${LC_CTYPE}" ]] && unset LC_CTYPE

## TODO: Make man pages colorful
## http://nion.modprobe.de/blog/archives/569-colored-manpages.html

## set LESS to empty, basically
export LESS=

function pathmunge() {
  [[ ! -d "${1}" ]] && return
  if ! [[ $PATH =~ (^|:)$1($|:) ]]; then
     if [ "$2" = "after" ] ; then
        PATH=$PATH:$1
     else
        PATH=$1:$PATH
     fi
  fi
}

function pathunmunge() {
  oldpath=("${(@s/:/)PATH}")
  newpath=""
  sep=""
  for p in ${oldpath[*]}; do
    if [[ "x${p}" != "x${1}" ]]; then
      newpath="${newpath}${sep}${p}"
      sep=":"
    fi
  done
  export PATH="${newpath}"
}

# Are we on Mac? Start the path as defined in /etc/paths
if [[ -x /usr/libexec/path_helper ]]; then
  eval `/usr/libexec/path_helper -s`
fi

# We need our bin folder
pathmunge "${HOME}/.bin"

# We need Go's bin folder
pathmunge "${GOPATH}/bin"

# Anything got installed into MYFS?
pathmunge "${MYFS}/bin"
pathmunge "${MYFS}/opt/go_appengine"
if [[ -d "${MYFS}" ]]; then
  if [[ -d "${MYFS}/opt" ]]; then
    for dir in `find "${MYFS}/opt" -maxdepth 1 -mindepth 1 -type d`; do
      if [[ -d "${dir}/bin" ]]; then
        pathmunge "${dir}/bin" after
      fi
    done
  fi

  # Make LD can find our files.
  export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:${HOME}/.filesystem/lib"
fi

# Add all rubygems bin dir
if [[ -d "${HOME}/.gem/ruby" ]]; then
  for dir in $HOME/.gem/ruby/*/bin; do
    pathmunge "${dir}"
  done
fi

# Export Github's token if it's readable.
github_token_path="$HOME/.github_token"
if [[ "$(uname)" = "Darwin" ]] && [[ -r "${github_token_path}" ]]; then
  export HOMEBREW_GITHUB_API_TOKEN=`head -1 ${github_token_path}`
fi
unset github_token_path

# Export MySQL credentials if it's readable
mysql_credentials_path="$HOME/.my.cnf"
if [[ -r "${mysql_credentials_path}" ]]; then
  user="`cat "$mysql_credentials_path" | grep "user" | cut -d= -f2 | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'`"
  pass="`cat "$mysql_credentials_path" | grep "password" | cut -d= -f2 | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//'`"

  if [ "x${user}" != "x" ]; then
    export MYSQL_USERNAME="${user}"
    export MYSQL_PASSWORD="${pass}"
  fi
  unset user pass
fi
unset mysql_credentials_path

# Export camlistore's secret keyring
# [[ -x "$(brew --prefix)/bin/camput" ]] && export CAMLI_SECRET_RING="${HOME}/.gnupg/secring.gpg"

# On ArchLinux disable pinentry for lastpass
if test -r /etc/lsb-release && grep -q 'DISTRIB_ID=Arch' /etc/lsb-release; then
  export LPASS_DISABLE_PINENTRY=1
fi

# use git ls-tree to speed up FZF. Fall back to find if no current folder is
# not under Git.
export FZF_DEFAULT_COMMAND='
  (git ls-tree -r --name-only HEAD ||
   find . -path "*/\.*" -prune -o -type f -print -o -type l -print |
      sed s/^..//) 2> /dev/null'

# on Linux, fix the look and feel of Java applications
if [[ "$(uname)" = "Linux" ]]; then
  export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
fi

#####################################################################
# git-extensions
#####################################################################

function gcim() {
  local message="${@}"
  local project=""
  local branch=""
  local story=""

  if [[ "x$(git rev-parse --git-dir 2> /dev/null)" != "x" ]]; then
    branch="$(current_branch)"
    story="$(echo "${branch}" | grep '^[A-Z][A-Z]*-[[:digit:]][[:digit:]]*$')"
  else
    echo "You must be under a git repository to use gcim"
    return 1
  fi

  if [[ -z "${message}" ]]; then
    echo "USAGE: gcim <message>"
    return 1
  fi

  if [[ "x${story}" != "x" ]]; then
    git commit -m "${message} (${story})"
  else
    git commit -m "${message}"
  fi
}

function gtime() {
  local branch=""

  if [[ "x$(git rev-parse --git-dir 2> /dev/null)" != "x" ]]; then
    branch="$(current_branch)"
  else
    echo "You must be under a git repository to use gcim"
    return 1
  fi

  git reflog --date=relative --all | grep "refs/heads/${branch}@.*: branch: Created" | sed -e 's:.*{\([^}]*\)}.*:\1:g'
}

function gorder() {
  local branch

  for branch in $(git branch | sed s/^..//); do
    echo -e "$(git log -1 --pretty=format:"%Cgreen%ci %Cblue%cr%Creset" "${branch}")\t${branch}"
  done | sort -r
}

# github_commit_link [REF]
#
# Print the Github commit link for REF (default HEAD)
github_commit_link() { # [REF]
  # get the reference we need
  local ref="${1:-HEAD}"
  # compute the path to the root of the repository
  local repo_dir="$(git rev-parse --show-toplevel)"
  # validate we are under a Github repo
  # TODO: must check the URL of the origin if this failed for repos outside
  # $GOPATH
  if ! echo "${repo_dir}" | grep -q -i '/github\.com/[^/]*/[^/]*'; then
    print_error 0 "this only works for Github.com"
    return 1
  fi
  # compute the user from the path
  local user="$(basename "$(dirname "${repo_dir}")")"
  # compute the repo from the path
  local repo="$(basename "${repo_dir}")"
  # get the commit from the ref
  local commit="$(git show --no-patch --format="%H" "${ref}")"
  # finally echo it
  echo "https://github.com/${user}/${repo}/commit/${commit}"
}

#####################################################################
# history settings
#####################################################################

## Command history configuration

[[ -z $HISTFILE ]] && export HISTFILE=${ZDOTDIR:-$HOME}/.zsh_history
export HISTSIZE=100000
export SAVEHIST=100000

setopt append_history
setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups # ignore duplication command history list
setopt hist_ignore_space
setopt hist_verify
setopt inc_append_history
setopt share_history # share command history data

#####################################################################
# Key bindings
#####################################################################

# http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html
# http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#Zle-Builtins
# http://zsh.sourceforge.net/Doc/Release/Zsh-Line-Editor.html#Standard-Widgets

# Make sure that the terminal is in application mode when zle is active, since
# only then values from $terminfo are valid
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
  function zle-line-init() {
    echoti smkx
  }
  function zle-line-finish() {
    echoti rmkx
  }
  zle -N zle-line-init
  zle -N zle-line-finish
fi

bindkey -v                                            # Use vim key bindings
export KEYTIMEOUT=1                                   # kill ZSH's lag when ESC is pressed in vim mode

bindkey '\ew' kill-region                             # [Esc-w] - Kill from the cursor to the mark
bindkey -s '\el' 'ls\n'                               # [Esc-l] - run command: ls
bindkey '^r' history-incremental-search-backward      # [Ctrl-r] - Search backward incrementally for a specified string. The string may begin with ^ to anchor the search to the beginning of the line.
if [[ "${terminfo[kpp]}" != "" ]]; then
  bindkey "${terminfo[kpp]}" up-line-or-history       # [PageUp] - Up a line of history
fi
if [[ "${terminfo[knp]}" != "" ]]; then
  bindkey "${terminfo[knp]}" down-line-or-history     # [PageDown] - Down a line of history
fi

if [[ "${terminfo[kcuu1]}" != "" ]]; then
  bindkey "${terminfo[kcuu1]}" up-line-or-search      # start typing + [Up-Arrow] - fuzzy find history forward
fi
if [[ "${terminfo[kcud1]}" != "" ]]; then
  bindkey "${terminfo[kcud1]}" down-line-or-search    # start typing + [Down-Arrow] - fuzzy find history backward
fi

if [[ "${terminfo[khome]}" != "" ]]; then
  bindkey "${terminfo[khome]}" beginning-of-line      # [Home] - Go to beginning of line
fi
if [[ "${terminfo[kend]}" != "" ]]; then
  bindkey "${terminfo[kend]}"  end-of-line            # [End] - Go to end of line
fi

bindkey ' ' magic-space                               # [Space] - do history expansion

bindkey '^[[1;5C' forward-word                        # [Ctrl-RightArrow] - move forward one word
bindkey '^[[1;5D' backward-word                       # [Ctrl-LeftArrow] - move backward one word

if [[ "${terminfo[kcbt]}" != "" ]]; then
  bindkey "${terminfo[kcbt]}" reverse-menu-complete   # [Shift-Tab] - move through the completion menu backwards
fi

bindkey '^?' backward-delete-char                     # [Backspace] - delete backward
if [[ "${terminfo[kdch1]}" != "" ]]; then
  bindkey "${terminfo[kdch1]}" delete-char            # [Delete] - delete forward
else
  bindkey "^[[3~" delete-char
  bindkey "^[3;5~" delete-char
  bindkey "\e[3~" delete-char
fi

# Edit the current command line in $EDITOR
autoload -U edit-command-line
zle -N edit-command-line
bindkey -M vicmd v edit-command-line

#####################################################################
# misc
#####################################################################

## smart urls
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

## file rename magick
bindkey "^[m" copy-prev-shell-word

## jobs
setopt long_list_jobs


#####################################################################
# termsupport
#####################################################################

#usage: title short_tab_title looooooooooooooooooooooggggggg_windows_title
#http://www.faqs.org/docs/Linux-mini/Xterm-Title.html#ss3.1
#Fully support screen, iterm, and probably most modern xterm and rxvt
#Limited support for Apple Terminal (Terminal can't set window or tab separately)
function title {
  if [[ "$DISABLE_AUTO_TITLE" == "true" ]] || [[ "$EMACS" == *term* ]]; then
    return
  fi
  if [[ "$TERM" == screen* ]]; then
    print -Pn "\ek$1:q\e\\" #set screen hardstatus, usually truncated at 20 chars
  elif [[ "$TERM" == xterm* ]] || [[ $TERM == rxvt* ]] || [[ $TERM == ansi ]] || [[ "$TERM_PROGRAM" == "iTerm.app" ]]; then
    print -Pn "\e]2;$2:q\a" #set window name
    print -Pn "\e]1;$1:q\a" #set icon (=tab) name (will override window name on broken terminal)
  fi
}

ZSH_THEME_TERM_TAB_TITLE_IDLE="%15<..<%~%<<" #15 char left truncated PWD
ZSH_THEME_TERM_TITLE_IDLE="%n@%m: %~"

#Appears when you have the prompt
function omz_termsupport_precmd {
  title $ZSH_THEME_TERM_TAB_TITLE_IDLE $ZSH_THEME_TERM_TITLE_IDLE
}

#Appears at the beginning of (and during) of command execution
function omz_termsupport_preexec {
  emulate -L zsh
  setopt extended_glob

  # cmd name only, or if this is sudo or ssh, the next cmd
  local CMD=${1[(wr)^(*=*|sudo|ssh|rake|-*)]:gs/%/%%}
  local LINE="${2:gs/%/%%}"

  title '$CMD' '%100>...>$LINE%<<'
}

autoload -U add-zsh-hook
add-zsh-hook precmd  omz_termsupport_precmd
add-zsh-hook preexec omz_termsupport_preexec

# Make sure TERM is sane
export TERM=xterm-256color
[ -n "$TMUX" ] && export TERM=screen-256color
if [[ "$(uname)" = "Linux" ]]; then
  export TERM=xterm-termite
fi

#####################################################################
# Externals
#####################################################################

if [[ -x $(which brew 2>/dev/null) ]]; then
  # Load autojump
  [[ -s "$(brew --prefix)/etc/profile.d/autojump.sh" ]] && source "$(brew --prefix)/etc/profile.d/autojump.sh"

  # Export CFLAGS and LDFLAGS
  export CGO_CFLAGS="-I/usr/local/include"
  export CGO_CPPFLAGS="${CGO_CFLAGS}"
  export CGO_CXXFLAGS="${CGO_CFLAGS}"
  export CGO_LDFLAGS="-L/usr/local/lib"
fi

# Load TheFuck
[[ -x "$(which thefuck 2>/dev/null)" ]] && eval "$(thefuck --alias)"

# Load iterm2 shell integration
[[ -r "${HOME}/.iterm2_shell_integration.zsh" ]] && source "${HOME}/.iterm2_shell_integration.zsh"

# Load travis
[[ -r "${HOME}/.travis/travis.sh" ]] && source "${HOME}/.travis/travis.sh"

# Load FZF
[[ -f "${HOME}/.fzf.zsh" ]] && source "${HOME}/.fzf.zsh"

# Load SSH agents
[[ -x "${HOME}/.bin/ssh-agents" ]] && eval `ssh-agents $SHELL`
#
# Load rbenv
if [[ -d "${HOME}/.rbenv" ]]; then
  pathmunge "${HOME}/.rbenv/bin"
  eval "$(rbenv init --no-rehash -)"
fi

# Load pyenv
if [[ -d "${HOME}/.pyenv" ]]; then
  pathmunge "${HOME}/.pyenv/bin"
  eval "$(pyenv init --no-rehash -)"
fi

# Load nvm
if [[ -f "/usr/share/nvm/init-nvm.sh" ]]; then
  nvm_init="/usr/share/nvm/init-nvm.sh"
elif [[ -f "${HOME}/.nvm/nvm.sh" ]]; then
  nvm_init="${HOME}/.nvm/nvm.sh"
fi
if [[ -n "${nvm_init}" ]]; then
  if [[ -z "${PUBLICA_NPM_TOKEN}" ]]; then
    # set the PUBLICA_NPM_TOKEN to a bogus value, it will be loaded by the
    # publica profile when it gets loaded
    export PUBLICA_NPM_TOKEN="undefined"
  fi
  source "${nvm_init}"

  # if a folder contains an .nvmrc, respect it
  autoload -U add-zsh-hook
  add-zsh-hook chpwd load_nvmrc
  load_nvmrc
fi
unset nvm_init

# load k8s completion
if [[ -x $(which kubectl 2>/dev/null) ]]; then
  source <(kubectl completion zsh)
fi

# load the Emscripten environment
if [[ -d "/usr/lib/emsdk" ]]; then
  pathmunge "/usr/lib/emsdk"
fi

#####################################################################
# Profile support
#####################################################################

# If the ACTIVE_PROFILE is set, source the profile file and activate the profile
if [[ -n "${ACTIVE_PROFILE}" ]]; then
  source "${ZSH}/profiles/${ACTIVE_PROFILE}.zsh"
  pactivate
fi

#####################################################################
# host overrides
#####################################################################

# Source host-specific settings if the exists. Keep this last so it can
# overwrite any of the other settings.
[[ -r "${ZSH}/hosts/${SHORT_HOST}.zsh" ]] && source "${ZSH}/hosts/${SHORT_HOST}.zsh"
