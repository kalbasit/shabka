#####################################################################
# core
#####################################################################

# macOS's $HOST changes with dhcp, etc. Use ComputerName if possible.
[[ "$OSTYPE" = darwin* ]] && SHORT_HOST="$( scutil --get ComputerName 2>/dev/null )"
[[ -z "${SHORT_HOST}" ]] && SHORT_HOST="${HOST/.*/}"

#####################################################################
# zplug
#####################################################################

# Load zplug
if [[ -r "${HOME}/.zplug/init.zsh" ]]; then
  source "${HOME}/.zplug/init.zsh"
else
  # instructions on how to get zplug installed
  echo -e "zplug cannot be found at ${HOME}/.zplug/init.zsh"
  echo -e "run the following command and then restart your shell"
  echo -e "git clone https://github.com/zplug/zplug.git /tmp/zplug && source /tmp/zplug/init.zsh && source ~/.zshrc && zplug install"
  # stop here but only if zplug is not available. It can only be avaliable here
  # if zplug was sourced but `zplug install` was not called yet. See the
  # installation command above
  whence -w zplug | grep function || return 1
fi

# speed up zplug. See https://github.com/zplug/zplug/issues/368#issuecomment-282566102
__zplug::io::file::generate

# let zplug manage itself
zplug 'zplug/zplug', hook-build:'zplug --self-manage'

# load the shellder theme
zplug "kalbasit/shellder", as:theme

# plugins
zplug "Dbz/zsh-kubernetes"
zplug "b4b4r07/emoji-cli"
zplug "b4b4r07/enhancd",                     use:init.sh
zplug "denolfe/zsh-travis"
zplug "hcgraf/zsh-sudo"
zplug "jreese/zsh-titles"
zplug "molovo/tipz"
zplug "peterhurford/git-it-on.zsh"
zplug "plugins/command-not-found",           from:oh-my-zsh
zplug "plugins/extract",                     from:oh-my-zsh
zplug "plugins/git",                         from:oh-my-zsh
zplug "plugins/github",                      from:oh-my-zsh
zplug "plugins/history",                     from:oh-my-zsh
zplug "supercrabtree/k"
zplug "zlsun/solarized-man"
zplug "zsh-users/zsh-completions"
zplug "zsh-users/zsh-history-substring-search"
zplug "zsh-users/zsh-syntax-highlighting",   defer:2

# Then, source plugins and add commands to $PATH
zplug load

if zplug check b4b4r07/enhancd; then
  # setting if enhancd is available
  export ENHANCD_FILTER=fzf-tmux
fi

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
export EDITOR=nvim
export SUDO_EDITOR=nvim

# Set the notmuch config
export NOTMUCH_CONFIG="${HOME}/.mail/.notmuch/config"

# Set the pager
export PAGER=less

# Set the language support
export LANG=en_US.UTF-8
export LC_ALL="${LANG}"
[[ -n "${LC_CTYPE}" ]] && unset LC_CTYPE

# We need our bin folder
pathmunge "${HOME}/.bin"

# load the Emscripten environment
pathmunge "/usr/lib/emsdk"

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
    pathmunge "${dir}" after
  done
fi

# add rbenv
pathmunge "${HOME}/.rbenv/bin"

# add pyenv
pathmunge "${HOME}/.pyenv/bin"

# use git ls-tree to speed up FZF. Fall back to find if no current folder is
# not under Git.
export FZF_DEFAULT_COMMAND='
  (git ls-tree -r --name-only HEAD ||
   find . -path "*/\.*" -prune -o -type f -print -o -type l -print |
      sed s/^..//) 2> /dev/null'

#####################################################################
# aliases
#####################################################################

alias blaze=bazel
alias comp=docker-compose
alias e="${EDITOR:-vim}"
alias gl='github_commit_link'
alias http='http --print=HhBb'
alias irc='tmx -L irc irc'
alias ll="ls -la"
alias mach=docker-machine
alias pw="ps aux | grep -v grep | grep -e"
alias remove_created_containers="docker rm -v \$(docker ps -a -q -f status=created)"
alias remove_dangling_images="docker rmi \$(docker images -f dangling=true -q)"
alias remove_dead_containers="docker rm -v \$(docker ps -a -q -f status=exited)"
alias rot13="tr '[A-Za-z]' '[N-ZA-Mn-za-m]'"
alias rserve_this="ruby -rrack -e \"Rack::Handler::WEBrick.run Rack::Directory.new('.')\""
alias serve_this="python2 -m SimpleHTTPServer"
alias t='task'
alias ta='task add'
alias td='task project:dotfiles'
alias tm='task project:morning'
alias utf8test='curl -L https://github.com/tmux/tmux/raw/master/tools/UTF-8-demo.txt'
alias vi=e
alias vim=e

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

fpath+="${HOME}/.zsh/completions"

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
[[ "${terminfo[kcuu1]}" != "" ]] && bindkey "${terminfo[kcuu1]}" history-substring-search-up

# start typing + [Down-Arrow] - fuzzy find history backward
[[ "${terminfo[kcud1]}" != "" ]] && bindkey "${terminfo[kcud1]}" history-substring-search-down

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
# Colemak key bindings
#####################################################################

# Credit: https://bazaar.launchpad.net/~akiva/colemak.vim/trunk/view/head:/.zshrc

# vi-backward-char (unbound) (^H h ^?) (ESC-[D)
#   Move backward one character, without changing lines.
bindkey -M vicmd 'n' vi-backward-char
bindkey -M vicmd '^[n' vi-backward-char
bindkey -M visual 'n' vi-backward-char

# vi-forward-char (unbound) (space l) (ESC-[C)
#   Move forward one character.
bindkey -M vicmd 'o' vi-forward-char
bindkey -M vicmd '^[o' vi-forward-char
bindkey -M visual 'o' vi-forward-char

# up-line (unbound) (unbound) (unbound)
#   Move up a line in the buffer.
bindkey -M vicmd 'i' up-line-or-history
bindkey -M vicmd '^[i' history-substring-search-up
bindkey -M visual 'i' up-line-or-history

# down-line (unbound) (unbound) (unbound)
#   Move down a line in the buffer.
bindkey -M vicmd 'e' down-line-or-history
bindkey -M vicmd '^[e' history-substring-search-down
bindkey -M visual 'e' down-line-or-history

# vi-backward-blank-word (unbound) (B) (unbound)
#   Move backward one word, where a word is defined as a series of non-blank
#   characters.
bindkey -M vicmd 'l' vi-backward-blank-word
bindkey -M vicmd '^[l' vi-backward-blank-word
bindkey -M visual 'l' vi-backward-blank-word

# vi-forward-blank-word (unbound) (B) (unbound)
#   Move forward one word, where a word is defined as a series of non-blank
#   characters.
bindkey -M vicmd 'w' vi-forward-blank-word
bindkey -M vicmd '^[w' vi-forward-blank-word
bindkey -M visual 'w' vi-forward-blank-word

# vi-backward-blank-word-end (unbound) (gE) (unbound)
#   Move to the end of the previous word, where a word is defined as a series
#   of non-blank characters.
bindkey -M vicmd 'L' vi-backward-blank-word-end
bindkey -M vicmd '^[L' vi-backward-blank-word-end
bindkey -M visual 'L' vi-backward-blank-word-end

# vi-forward-blank-word-end (unbound) (E) (unbound)
#   Move to the end of the current word, or, if at the end of the current word,
#   to the end of the next word, where a word is defined as a series of
#   non-blank characters.
bindkey -M vicmd 'Y' vi-forward-blank-word-end
bindkey -M vicmd '^[Y' vi-forward-blank-word-end
bindkey -M visual 'Y' vi-forward-blank-word-end

# vi-repeat-search (unbound) (n) (unbound)
#   Repeat the last vi history search.
bindkey -M vicmd 'k' vi-repeat-search
bindkey -M vicmd '^[k' vi-repeat-search
bindkey -M isearch '^[k' vi-repeat-search
bindkey -M isearch '^[u' vi-repeat-search

# vi-rev-repeat-search (unbound) (N) (unbound)
#   Repeat the last vi history search, but in reverse.
bindkey -M vicmd 'K' vi-rev-repeat-search
bindkey -M vicmd '^[K' vi-rev-repeat-search
bindkey -M isearch '^[K' vi-rev-repeat-search
bindkey -M isearch '^[e' vi-rev-repeat-search

# vi-pound-insert
#   If there is no # character at the beginning of the buffer, add one to the
#   beginning of each line. If there is one, remove a # from each line that
#   has one. In either case, accept the current line. The INTERACTIVE_COMMENTS
#   option must be set for this to have any usefulness.
bindkey -M vicmd '#' vi-pound-insert
bindkey -M vicmd '^[#' vi-pound-insert
bindkey -M visual '#' vi-pound-insert

# vi-add-next (unbound) (a) (unbound)
#   Enter insert mode after the current cursor position, without changing lines.
bindkey -M vicmd 't' vi-add-next
bindkey -M vicmd '^[t' vi-add-next
bindkey -M visual 't' vi-add-next

# vi-add-eol (unbound) (A) (unbound)
#   Move to the end of the line and enter insert mode.
bindkey -M vicmd 'T' vi-add-eol
bindkey -M vicmd '^[T' vi-add-eol
bindkey -M visual 'T' vi-add-eol

# vi-change (unbound) (c) (unbound)
#   Read a movement command from the keyboard, and kill from the cursor
#   position to the endpoint of the movement. Then enter insert mode. If the
#   command is vi-change, change the current line. For compatibility with vi,
#   if the command is vi-forward-word or vi-forward-blank-word, the whitespace
#   after the word is not included. If you prefer the more consistent behaviour
#   with the whitespace included use the following key binding:
#     bindkey -a -s cw dwi
bindkey -M vicmd 'w' vi-change
bindkey -M vicmd '^[w' vi-change
bindkey -M visual 'w' vi-change

# vi-change-eol (unbound) (C) (unbound)
#  Kill to the end of the line and enter insert mode.
bindkey -M vicmd 'W' vi-change-eol
bindkey -M vicmd '^[W' vi-change-eol
bindkey -M visual 'W' vi-change-eol

# vi-change-whole-line (unbound) (S) (unbound)
#  Kill the current line and enter insert mode.
bindkey -M vicmd 'ww' vi-change-whole-line
bindkey -M vicmd '^[w^[w' vi-change-whole-line
bindkey -M visual 'ww' vi-change-whole-line

# vi-insert (unbound) (i) (unbound)
#   Enter insert mode.
bindkey -M vicmd 's' vi-insert
bindkey -M vicmd '^[s' vi-insert
bindkey -M visual 's' vi-insert

# vi-insert-bol (unbound) (I) (unbound)
#   Move to the first non-blank character on the line and enter insert mode.
bindkey -M vicmd 'S' vi-insert-bol
bindkey -M vicmd '^[S' vi-insert-bol
bindkey -M visual 'S' vi-insert-bol

# vi-open-line-above (unbound) (H) (unbound)
#   Open a line above the cursor and enter insert mode.
bindkey -M vicmd 'H' vi-open-line-above
bindkey -M vicmd '^[H' vi-open-line-above
bindkey -M visual 'H' vi-open-line-above

# vi-open-line-below (unbound) (h) (unbound)
#   Open a line below the cursor and enter insert mode.
bindkey -M vicmd 'h' vi-open-line-below
bindkey -M vicmd '^[h' vi-open-line-below
bindkey -M visual 'h' vi-open-line-below

# vi-yank (unbound) (y) (unbound)
#   Read a movement command from the keyboard, and copy the region from the
#   cursor position to the endpoint of the movement into the kill buffer. If
#   the command is vi-yank, copy the current line.
bindkey -M vicmd 'c' vi-yank
bindkey -M vicmd '^[c' vi-yank
bindkey -M visual 'c' vi-yank

# vi-yank-whole-line (unbound) (Y) (unbound)
#   Copy the current line into the kill buffer.
bindkey -M vicmd 'C' vi-yank-whole-line
bindkey -M vicmd '^[C' vi-yank-whole-line
bindkey -M visual 'C' vi-yank-whole-line


# vi-put-before (unbound) (P) (unbound)
#   Insert the contents of the kill buffer before the cursor. If the kill
#   buffer contains a sequence of lines (as opposed to characters), paste
#   it above the current line.
bindkey -M vicmd 'V' vi-put-before

# vi-put-after (unbound) (p) (unbound)
#  Insert the contents of the kill buffer after the cursor. If the kill buffer
#  contains a sequence of lines (as opposed to characters), paste it below the
#  current line.
bindkey -M vicmd 'v' vi-put-after

# visual-mode (unbound) (v) (unbound)
#  Toggle vim-style visual selection mode. If line-wise visual mode is
#  currently enabled then it is changed to being character-wise. If used
#  following an operator, it forces the subsequent movement command to be
#  treated as a character-wise movement.
bindkey -M vicmd 'a' visual-mode

# visual-line-mode (unbound) (V) (unbound)
#  Toggle vim-style line-wise visual selection mode. If character-wise visual
#  mode is currently enabled then it is changed to being line-wise. If used
#  following an operator, it forces the subsequent movement command to be
#  treated as a line-wise movement.
bindkey -M vicmd 'A' visual-line-mode

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

## enable comments
setopt interactivecomments


#####################################################################
# termsupport
#####################################################################

# Make sure TERM is sane
export TERM=xterm-256color
[[ -n "$TMUX" ]] && export TERM=screen-256color
[[ "$OSTYPE" = linux* ]] && export TERM=xterm-termite

#####################################################################
# Externals
#####################################################################

# Mac only externals
if [[ "$OSTYPE" = darwin* ]]; then
  if [[ -x $(which brew 2>/dev/null) ]]; then
    # Load autojump
    autojump_path="$(brew --prefix)/etc/profile.d/autojump.sh"
    [[ -r "${autojump_path}" ]] && source "${autojump_path}"
    unset autojump_path

    # Export CFLAGS and LDFLAGS
    export CGO_CFLAGS="-I/usr/local/include"
    export CGO_CPPFLAGS="${CGO_CFLAGS}"
    export CGO_CXXFLAGS="${CGO_CFLAGS}"
    export CGO_LDFLAGS="-L/usr/local/lib"
  fi

  # Load iterm2 shell integration
  [[ -r "${HOME}/.iterm2_shell_integration.zsh" ]] && source "${HOME}/.iterm2_shell_integration.zsh"

  # Load the docker machine environmen
  if [[ -x $(which docker-machine 2>/dev/null) ]]; then
    eval $(docker-machine env)
  fi
fi

# Load TheFuck
if [[ -x "$(which thefuck 2>/dev/null)" ]]; then
  # Lazy load TheFuck
  lazyLoadFuck () {
    unalias fuck
    unfunction lazyLoadFuck
    eval "$(thefuck --alias)"
  }
  alias fuck=lazyLoadFuck
fi

# Load travis
[[ -r "${HOME}/.travis/travis.sh" ]] && source "${HOME}/.travis/travis.sh"

# Load FZF
[[ -r "${HOME}/.fzf.zsh" ]] && source "${HOME}/.fzf.zsh"

# Load SSH agents
[[ -x "${HOME}/.bin/ssh-agents" ]] && eval "$( ssh-agents $SHELL )"

# Load rbenv
if [[ -d "${HOME}/.rbenv" ]]; then
  lazyLoadRbenv() {
    unalias rbenv
    unfunction lazyLoadRbenv
    eval "$(rbenv init --no-rehash -)"
  }
  alias rbenv=lazyLoadRbenv
fi

# Load pyenv
if [[ -d "${HOME}/.pyenv" ]]; then
  lazyLoadPyenv() {
    unalias pyenv
    unfunction lazyLoadPyenv
    eval "$(pyenv init --no-rehash -)"
  }
  alias pyenv=lazyLoadPyenv
fi

# Load nvm
[[ -f "/usr/share/nvm/init-nvm.sh" ]] && export NVM_INIT="/usr/share/nvm/init-nvm.sh"
[[ -f "${HOME}/.nvm/nvm.sh" ]] && export NVM_INIT="${HOME}/.nvm/nvm.sh"
if [[ -n "${NVM_INIT}" ]]; then
  # define the lazy loading
  lazyLoadNvm() {
    unalias nvm
    unfunction lazyLoadNvm
    unalias npm
    unfunction lazyLoadNpm

    # source the init
    source "${NVM_INIT}"

    # we do not need the NVM_INIT anymore
    unset NVM_INIT

    # if a folder contains an .nvmrc, respect it
    autoload -U add-zsh-hook
    add-zsh-hook chpwd load_nvmrc
  }
  alias nvm=lazyLoadNvm

  # lazyLoadNpm is a function to load nvm. It will only be called if
  # lazyLoadNvm was never called before.
  lazyLoadNpm() {
    # call lazyLoadNvm so it loads the actual npm, it will also remove this
    # function and the alias set to it.
    lazyLoadNvm

    # call the actual npm which is loaded by now
    npm $@
  }
  alias npm=lazyLoadNpm
else
  unset NVM_INIT
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

# load all the profiles
for p in ${HOME}/.zsh/profiles/*.zsh; do
  source "${p}"
  pload

  unfunction pcode pload pactivate pdeactivate
done

# If the ACTIVE_PROFILE is set, source the profile file and activate the profile
if [[ -n "${ACTIVE_PROFILE}" ]] && [[ -r "${HOME}/.zsh/profiles/${ACTIVE_PROFILE}.zsh" ]]; then
  source "${HOME}/.zsh/profiles/${ACTIVE_PROFILE}.zsh"
  pactivate

  unfunction pcode pload pactivate pdeactivate
fi
