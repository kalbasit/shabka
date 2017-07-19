# add the zsh functions to the fpath
fpath+="${HOME}/.zsh/functions"
fpath+="${HOME}/.zsh/completions"

# load the important functions
source "${HOME}/.zsh/functions/debug"
source "${HOME}/.zsh/functions/have"
source "${HOME}/.zsh/functions/is_func"
source "${HOME}/.zsh/functions/pathappend"
source "${HOME}/.zsh/functions/pathprepend"
source "${HOME}/.zsh/functions/pathunmunge"
source "${HOME}/.zsh/functions/sp"
source "${HOME}/.zsh/functions/ssh_agents"

# autoload all of the functions
for func in ${HOME}/.zsh/functions/*; do
  case "$(basename ${func})" in
    debug)       ;;
    have)        ;;
    is_func)     ;;
    pathappend)  ;;
    pathprepend) ;;
    pathunmunge) ;;
    sp)          ;;
    ssh_agents)  ;;
    *)
      local func_name="$(basename ${func})"
      autoload -U "${func_name}"
      ;;
  esac
done
unset func
