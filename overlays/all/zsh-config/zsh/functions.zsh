# add the zsh functions to the fpath
fpath+="@out_dir@/zsh/functions"
fpath+="@out_dir@/zsh/completions"

# load the important functions
source "@out_dir@/zsh/functions/debug"
source "@out_dir@/zsh/functions/have"
source "@out_dir@/zsh/functions/is_func"
source "@out_dir@/zsh/functions/pathappend"
source "@out_dir@/zsh/functions/pathprepend"
source "@out_dir@/zsh/functions/pathunmunge"
source "@out_dir@/zsh/functions/sp"
source "@out_dir@/zsh/functions/ssh_agents"

# autoload all of the functions
for func in @out_dir@/zsh/functions/*; do
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
