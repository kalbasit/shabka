#####################################################################
# core
#####################################################################

# Figure out the SHORT hostname
if [[ "$OSTYPE" = darwin* ]]; then
  # macOS's $HOST changes with dhcp, etc. Use ComputerName if possible.
  SHORT_HOST=$(scutil --get ComputerName 2>/dev/null) || SHORT_HOST=${HOST/.*/}
else
  SHORT_HOST=${HOST/.*/}
fi

#####################################################################
# zplug
#####################################################################

# if zplug is not installed, do it
if [[ -z "${TMUX}" ]]; then
  if [[ ! -f "${HOME}/.zplug/init.zsh" ]]; then
    if [[ -d "${HOME}/.zplug" ]]; then
      echo "FATAL: ${HOME}/.zplug is present but the init.zsh is not"
      return 1
    fi
    git clone https://github.com/zplug/zplug.git "${HOME}/.zplug"
  fi
fi

# Load zplug
source "${HOME}/.zplug/init.zsh"
# let zplug manage itself
zplug 'zplug/zplug', hook-build:'zplug --self-manage'

# load the shellder theme
zplug "simnalamburt/shellder", as:theme

# plugins
zplug "Dbz/zsh-kubernetes"
zplug "b4b4r07/emoji-cli"
zplug "b4b4r07/enhancd",           use:init.sh
zplug "denolfe/zsh-travis"
zplug "hcgraf/zsh-sudo"
zplug "jreese/zsh-titles"
zplug "molovo/tipz"
zplug "peterhurford/git-it-on.zsh"
zplug "plugins/command-not-found", from:oh-my-zsh
zplug "plugins/extract",           from:oh-my-zsh
zplug "plugins/git",               from:oh-my-zsh
zplug "plugins/github",            from:oh-my-zsh
zplug "plugins/history",           from:oh-my-zsh
zplug "supercrabtree/k"
zplug "zlsun/solarized-man"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-syntax-highlighting"

# Install any missing zplug plugin
if [[ -z "${TMUX}" ]]; then
  zplug check || zplug install
fi

# Then, source plugins and add commands to $PATH
zplug load

#####################################################################
# functions
#####################################################################

for func in ${HOME}/.zsh/functions/*.zsh; do
    # shellcheck disable=SC1090
    source "${func}"
done

#####################################################################
# exports
#####################################################################

if [[ "$OSTYPE" = linux* ]]; then
  # GPG_TTY is needed for gpg with pinentry-curses
  export GPG_TTY="$(tty)"
  # disable pinentry in lastpass
  export LPASS_DISABLE_PINENTRY=1
  # fix the look of Java applications
  export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
fi

if [[ "$OSTYPE" = darwin* ]]; then
  # export system-wide defined PATH
  eval "$(/usr/libexec/path_helper -s)"

  # Export Github's token if it's readable.
  [[ -r "${HOME}/.github_token" ]] && \
    export HOMEBREW_GITHUB_API_TOKEN="$(head -1 "${HOME}/.github_token")"
fi

# Set the browser to my relay browser
export BROWSER="${HOME}/.bin/relay-browser"

# Set the GOPATH
export GLOBAL_GOPATH="${HOME}/code"
export GOPATH="${GLOBAL_GOPATH}"

# Set MYFS to my filesysemt
export MYFS="${HOME}/.filesystem"

# Set the editor
export EDITOR="$(which nvim)"
export SUDO_EDITOR="$(which nvim)"

# Set the notmuch config
export NOTMUCH_CONFIG="${HOME}/.mail/.notmuch/config"

# Set the pager
export PAGER=most

# Set the language support
export LANG=en_US.UTF-8
export LC_ALL="${LANG}"
[[ -n "${LC_CTYPE}" ]] && unset LC_CTYPE

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

# use git ls-tree to speed up FZF. Fall back to find if no current folder is
# not under Git.
export FZF_DEFAULT_COMMAND='
  (git ls-tree -r --name-only HEAD ||
   find . -path "*/\.*" -prune -o -type f -print -o -type l -print |
      sed s/^..//) 2> /dev/null'

#####################################################################
# aliases
#####################################################################

# Conditional aliases
[[ -x "`which ack-grep 2> /dev/null`" ]] && alias ack="ack-grep -il"

# Aliases
alias blaze=bazel
alias comp=docker-compose
alias e="${EDITOR:-vim}"
alias gl='github_commit_link'
alias http='http --print=HhBb'
alias irc='tmux attach -t irc || tmux new -s irc irssi'
alias mach=docker-machine
alias pw="ps aux | grep -v grep | grep -e"
alias remove_created_containers="docker rm -v \$(docker ps -a -q -f status=created)"
alias remove_dangling_images="docker rmi \$(docker images -f "dangling=true" -q)"
alias remove_dead_containers="docker rm -v \$(docker ps -a -q -f status=exited)"
alias rot13="tr '[A-Za-z]' '[N-ZA-Mn-za-m]'"
alias rserve_this="ruby -rrack -e \"Rack::Handler::WEBrick.run Rack::Directory.new('.')\""
alias serve_this="python2 -m SimpleHTTPServer"
alias t='task'
alias ta='task add'
alias tm='task project:morning'
alias utf8test='curl -L https://github.com/tmux/tmux/raw/master/tools/UTF-8-demo.txt'
alias vi="${EDITOR:-vim}"
alias vim="${EDITOR:-vim}"

# Always enable colored `grep` output
# Note: `GREP_OPTIONS="--color=auto"` is deprecated, hence the alias usage.
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias grep='grep --color=auto'

# General aliases
alias -g rocker_auth="--auth kalbasit:\$(lpass show --password 4984935876)"

# Mac only
if [[ "$OSTYPE" = darwin* ]]; then
  alias mac_install_cert='sudo security add-trusted-cert -d -r trustRoot -k /Library/Keychains/System.keychain'
  alias upup='brew upgrade'
fi

# Linux only
if [[ "$OSTYPE" = linux* ]]; then
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

# Find the option for using colors in ls, depending on the version
if [[ "$OSTYPE" == netbsd* ]]; then
  # On NetBSD, test if "gls" (GNU ls) is installed (this one supports colors);
  # otherwise, leave ls as is, because NetBSD's ls doesn't support -G
  gls --color -d . &>/dev/null && alias ls='gls --color=tty'
elif [[ "$OSTYPE" == openbsd* ]]; then
  # On OpenBSD, "gls" (ls from GNU coreutils) and "colorls" (ls from base,
  # with color and multibyte support) are available from ports.  "colorls"
  # will be installed on purpose and can't be pulled in by installing
  # coreutils, so prefer it to "gls".
  gls --color -d . &>/dev/null && alias ls='gls --color=tty'
  colorls -G -d . &>/dev/null && alias ls='colorls -G'
elif [[ "$OSTYPE" == darwin* ]]; then
  # this is a good alias, it works by default just using $LSCOLORS
  ls -G . &>/dev/null && alias ls='ls -G'

  # only use coreutils ls if there is a dircolors customization present ($LS_COLORS or .dircolors file)
  # otherwise, gls will use the default color scheme which is ugly af
  [[ -n "$LS_COLORS" || -f "$HOME/.dircolors" ]] && gls --color -d . &>/dev/null && alias ls='gls --color=tty'
else
  # For GNU ls, we use the default ls color theme. They can later be overwritten by themes.
  if [[ -z "$LS_COLORS" ]]; then
    (( $+commands[dircolors] )) && eval "$(dircolors -b)"
  fi

  ls --color -d . &>/dev/null && alias ls='ls --color=tty' || { ls -G . &>/dev/null && alias ls='ls -G' }

  # Take advantage of $LS_COLORS for completion as well.
  zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
fi

#####################################################################
# completion
#####################################################################

unsetopt menu_complete   # do not autoselect the first completion entry
unsetopt flowcontrol
setopt auto_menu         # show completion menu on succesive tab press
setopt complete_in_word
setopt always_to_end

# allow selection of the item in the autocomplete menu
zstyle ':completion:*:*:*:*:*' menu select

zstyle ':completion:*' matcher-list 'm:{a-zA-Z-_}={A-Za-z_-}' 'r:|=*' 'l:|=* r:|=*'

zstyle ':completion:*' list-colors ''
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"

# disable named-directories autocompletion
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories

# Don't complete uninteresting users
zstyle ':completion:*:*:*:users' ignored-patterns \
        adm amanda apache at avahi avahi-autoipd beaglidx bin cacti canna \
        clamav daemon dbus distcache dnsmasq dovecot fax ftp games gdm \
        gkrellmd gopher hacluster haldaemon halt hsqldb ident junkbust kdm \
        ldap lp mail mailman mailnull man messagebus  mldonkey mysql nagios \
        named netdump news nfsnobody nobody nscd ntp nut nx obsrun openvpn \
        operator pcap polkitd postfix postgres privoxy pulse pvm quagga radvd \
        rpc rpcuser rpm rtkit scard shutdown squid sshd statd svn sync tftp \
        usbmux uucp vcsa wwwrun xfs '_*'

# ... unless we really want to.
zstyle '*' single-ignored show

# Show dots while doing completion
expand-or-complete-with-dots() {
  # toggle line-wrapping off and back on again
  [[ -n "$terminfo[rmam]" && -n "$terminfo[smam]" ]] && echoti rmam
  print -Pn "%{%F{red}......%f%}"
  [[ -n "$terminfo[rmam]" && -n "$terminfo[smam]" ]] && echoti smam

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
setopt cdablevars

alias ..='cd ..'
alias cd..='cd ..'
alias cd...='cd ../..'
alias cd....='cd ../../..'
alias cd.....='cd ../../../..'
alias cd/='cd /'

alias md='mkdir -p'
alias rd=rmdir
alias d='dirs -v | head -10'

#####################################################################
# history settings
#####################################################################

## Command history configuration

[[ -z $HISTFILE ]] && export HISTFILE=${ZDOTDIR:-$HOME}/.zsh_history
export HISTSIZE=100000
export SAVEHIST=100000

# show dates as "mm/dd/yyyy"
# use 'fc -El 1' for "dd.mm.yyyy"
# use 'fc -il 1' for "yyyy-mm-dd"
alias history='fc -fl 1'

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

bindkey '^r' history-incremental-search-backward      # [Ctrl-r] - Search backward incrementally for a specified string. The string may begin with ^ to anchor the search to the beginning of the line.
if [[ "${terminfo[kpp]}" != "" ]]; then
  bindkey "${terminfo[kpp]}" up-line-or-history       # [PageUp] - Up a line of history
fi
if [[ "${terminfo[knp]}" != "" ]]; then
  bindkey "${terminfo[knp]}" down-line-or-history     # [PageDown] - Down a line of history
fi

# start typing + [Up-Arrow] - fuzzy find history forward
if [[ "${terminfo[kcuu1]}" != "" ]]; then
  autoload -U up-line-or-beginning-search
  zle -N up-line-or-beginning-search
  bindkey "${terminfo[kcuu1]}" up-line-or-beginning-search
fi
# start typing + [Down-Arrow] - fuzzy find history backward
if [[ "${terminfo[kcud1]}" != "" ]]; then
  autoload -U down-line-or-beginning-search
  zle -N down-line-or-beginning-search
  bindkey "${terminfo[kcud1]}" down-line-or-beginning-search
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
bindkey "^y" copy-prev-shell-word

## jobs
setopt long_list_jobs


#####################################################################
# termsupport
#####################################################################

# Make sure TERM is sane
export TERM=xterm-256color
[ -n "$TMUX" ] && export TERM=screen-256color
if [[ "$OSTYPE" = linux* ]]; then
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

if [[ "$OSTYPE" = darwin* ]]; then
  # Load iterm2 shell integration
  [[ -r "${HOME}/.iterm2_shell_integration.zsh" ]] && source "${HOME}/.iterm2_shell_integration.zsh"
fi

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
# host overrides
#####################################################################

# Source host-specific settings if the exists. Keep this last so it can
# overwrite any of the other settings.
[[ -r "${HOME}/.zsh/hosts/${SHORT_HOST}.zsh" ]] && source "${HOME}/.zsh/hosts/${SHORT_HOST}.zsh"

#####################################################################
# Profile support
#####################################################################

# If the ACTIVE_PROFILE is set, source the profile file and activate the profile
if [[ -n "${ACTIVE_PROFILE}" ]]; then
  source "${HOME}/.zsh/profiles/${ACTIVE_PROFILE}.zsh"
  pactivate
fi
