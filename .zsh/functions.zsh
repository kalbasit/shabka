# add the zsh functions to the fpath
fpath+="${HOME}/.zsh/functions"
fpath+="${HOME}/.zsh/completions"

# load the important functions
source "${HOME}/.zsh/functions/debug"
source "${HOME}/.zsh/functions/have"
source "${HOME}/.zsh/functions/is_func"
source "${HOME}/.zsh/functions/sp"
source "${HOME}/.zsh/functions/ssh-agents"

# autoload all of the functions
for func in ${HOME}/.zsh/functions/*; do
  case "$(basename ${func})" in
    debug)      ;;
    have)       ;;
    is_func)    ;;
    sp)         ;;
    ssh-agents) ;;
    *)
      autoload -U "$(basename ${func})"
      ;;
  esac
done
unset func
