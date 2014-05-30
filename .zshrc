# ZSHRC

# Which theme to use
THEME="crunch"

# Define paths
ZSH="${HOME}/.zsh"
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
ZSH_COMPDUMP="${HOME}/.zcompdump-${SHORT_HOST}-${ZSH_VERSION}"

# Load and run compinit
autoload -U compinit
compinit -i -d "${ZSH_COMPDUMP}"

##############

typeset -Ug path # Make sure the path array does not contain duplicates
[[ -x /usr/libexec/path_helper ]] && eval `/usr/libexec/path_helper -s`
if [[ -d $HOME/.filesystem/bin ]]; then
  path+=($HOME/.filesystem/bin(N-/))
  export DYLD_LIBRARY_PATH=$HOME/.filesystem/bin:$DYLD_LIBRARY_PATH
fi
path+=($HOME/code/go/bin(N-/))
path+=($HOME/.cask/bin(N-/))
for i in $HOME/.filesystem/opt/*; do
  path+=($i/bin(N-/))
done
for i in $HOME/.filesystem/opt/*; do
  export DYLD_LIBRARY_PATH=$i/lib:$DYLD_LIBRARY_PATH
done
if [[ -d /brew ]]; then
  path=(/brew/bin $path)
  export DYLD_LIBRARY_PATH=/brew/lib:$DYLD_LIBRARY_PATH
fi

# Load Google specific stuff
[[ -r "$HOME/.zshrc-google" ]] && source "$HOME/.zshrc-google"

# Load travis
[[ -r "$HOME/.travis/travis.sh" ]] && source "$HOME/.travis/travis.sh"

# Load rbenv
if [[ -d $HOME/.rbenv ]]; then
  path=($HOME/.rbenv/bin(N-/) $path)
  eval "$(rbenv init --no-rehash - zsh)"
fi

# Load pyenv
if [[ -d $HOME/.pyenv ]]; then
  path=($HOME/.pyenv/bin(N-/) $path)
  eval "$(pyenv init --no-rehash - zsh)"
fi

##############

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
