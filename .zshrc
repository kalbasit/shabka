# ZSHRC

# Which theme to use
THEME="blinks"

# Define paths
ZSH="${ZDOTDIR:-$HOME}/.zsh"
PLUGINS_PATH="${ZSH}/plugins"
THEMES_PATH="${ZSH}/themes"

# Get the list of configs
configs=()
for config (${ZSH}/*.zsh); do
  configs=($(basename $config) $configs)
done

# Get the list of plugins
plugins=()
for plugin (${PLUGINS_PATH}/*); do
  plugins=($(basename $plugin) $plugins)
done

# Add each plugin to fpath
for plugin ($plugins); do
  fpath=("${PLUGINS_PATH}/$plugin" $fpath)
done

# Figure out the SHORT hostname
if [ -n "$commands[scutil]" ]; then
  # OS X
  SHORT_HOST=$(scutil --get ComputerName)
else
  SHORT_HOST=${HOST/.*/}
fi

# Save the location of the current completion dump file.
ZSH_COMPDUMP="${ZDOTDIR:-$HOME}/.zcompdump-${SHORT_HOST}-${ZSH_VERSION}"

# Load and run compinit
autoload -U compinit
compinit -i -d "${ZSH_COMPDUMP}"

# Load all the configs
for config ($configs); do
  source "${ZSH}/${config}"
done

# Load all the plugins
for plugin ($plugins); do
  plugin_path="${PLUGINS_PATH}/${plugin}/${plugin}.plugin.zsh"
  if [[ -r "${plugin_path}" ]]; then
    source "${PLUGINS_PATH}/${plugin}/${plugin}.plugin.zsh"
  fi
done

# Load the theme
source "${THEMES_PATH}/${THEME}.zsh-theme"

# Load Dailymotion specific stuff
[[ -r "${ZDOTDIR:-$HOME}/.zshrc.dailymotion" ]] && source "${ZDOTDIR:-$HOME}/.zshrc.dailymotion"

# Load travis
[[ -r "${HOME}/.travis/travis.sh" ]] && source "${HOME}/.travis/travis.sh"

export TERM=xterm-256color
[ -n "$TMUX" ] && export TERM=screen-256color

# Load SSH agents
eval `ssh-agents $SHELL`
