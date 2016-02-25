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
  if [[ "`hostname`" = "hermes" ]]; then
    echo "%{%F{red}%}"
  else
    echo "%{%F{cyan}%}"
  fi
}

ZSH_THEME_GIT_PROMPT_PREFIX=" [%{%F{blue}%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{%f%k%F{green}%}]"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{%F{red}%}*%{%f%k%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""

PROMPT='%{%F{green}%}%n%{%F{blue}%}@$(_host_color)%m%{%F{green}%} %{%F{yellow}%}%~%{%F{green}%}$(git_prompt_info)$(_work_prompt_info)%E%{%f%k%} $(_prompt_char) %#%{%f%k%} '
