# ZSHRC

# Which theme to use
THEME="kalbasit"

# Define paths
ZSH="${ZDOTDIR:-$HOME}/.zsh"
PLUGINS_PATH="${ZSH}/plugins"
THEMES_PATH="${ZSH}/themes"

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

# Load all the configs
for config (${ZSH}/*.zsh); do
  source "${config}"
done
unset config

# Tell the completion where the plugins are
for plugin (${PLUGINS_PATH}/*); do
  fpath=("$plugin" $fpath)
done
unset plugin

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

# Load all the plugins
for plugin (${PLUGINS_PATH}/*); do
  plugin="$(basename $plugin)"
  plugin_path="${PLUGINS_PATH}/${plugin}/${plugin}.plugin.zsh"
  [[ -r "${plugin_path}" ]] && source "${plugin_path}"
done
unset plugin plugin_path

# Load the theme
source "${THEMES_PATH}/${THEME}.zsh-theme"

# Make sure TERM is sane
export TERM=xterm-256color
[ -n "$TMUX" ] && export TERM=screen-256color

# If the ACTIVE_PROFILE is set, source the profile file and activate the profile
if [[ -n "${ACTIVE_PROFILE}" ]]; then
  source "${ZSH}/profiles/${ACTIVE_PROFILE}.zsh"
  pactivate
fi

# Source host-specific settings if the exists. Keep this last so it can
# overwrite any of the other settings.
[[ -r "${ZSH}/hosts/${SHORT_HOST}.zsh" ]] && source "${ZSH}/hosts/${SHORT_HOST}.zsh"

## END
