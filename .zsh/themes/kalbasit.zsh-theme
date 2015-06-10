function _prompt_char() {
  if $(git rev-parse --is-inside-work-tree >/dev/null 2>&1); then
    echo "%{%F{blue}%}±%{%f%k%}"
  else
    echo ''
  fi
}

function work_prompt_info() {
  if [[ -n "${ACTIVE_WORK_PROFILE}" ]]; then
    echo "%{%F{green}%} {${ACTIVE_WORK_PROFILE}}%{%f%k%}"
  else
    echo ''
  fi
}

ZSH_THEME_GIT_PROMPT_PREFIX=" [%{%F{blue}%}"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{%f%k%F{green}%}]"
ZSH_THEME_GIT_PROMPT_DIRTY=" %{%F{red}%}*%{%f%k%}"
ZSH_THEME_GIT_PROMPT_CLEAN=""

PROMPT='%{%F{green}%}%n%{%F{blue}%}@%{%F{cyan}%}%m%{%F{green}%} %{%F{yellow}%}%~%{%F{green}%}$(git_prompt_info)$(work_prompt_info)%E%{%f%k%} $(_prompt_char) %#%{%f%k%} '
