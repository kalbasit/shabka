function _prompt_char() {
  if $(git rev-parse --is-inside-work-tree >/dev/null 2>&1); then
    echo "%{%F{blue}%}±%{%f%k%}"
  else
    echo ''
  fi
}

function _work_prompt_info() {
  if [[ -n "${ACTIVE_PROFILE}" ]]; then
    echo "%{%F{green}%} {${ACTIVE_PROFILE}}%{%f%k%}"
  else
    echo ''
  fi
}

function _host_color() {
  if [[ "`hostname`" = "zeus" ]]; then
    echo "%{%F{red}%}"
  else
    echo "%{%F{cyan}%}"
  fi
}

# _current_path returns the current path with every parent folder represented
# by only the first char.
# https://github.com/robbyrussell/oh-my-zsh/issues/5068
function _current_path() {
  echo /${(j:/:)${(M)${(s:/:)PWD:h}#(|.)[^.]}}/${PWD:t}
}

ZSH_THEME_GIT_PROMPT_PREFIX=" [%{%F{blue}%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{%f%k%F{green}%}]"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{%F{red}%}*%{%f%k%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""

PROMPT='%{%F{green}%}%n%{%F{blue}%}@$(_host_color)%m%{%F{green}%} %{%F{yellow}%}$(_current_path)%{%F{green}%}$(git_prompt_info)$(_work_prompt_info)%E%{%f%k%} $(_prompt_char) %#%{%f%k%} '
