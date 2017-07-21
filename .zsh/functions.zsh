# compute the current location
cur_dir="$(cd "$(dirname "${0}")" && pwd)"

# add the zsh functions to the fpath
fpath+="${cur_dir}/functions"
fpath+="${cur_dir}/completions"

# load the important functions
source "${cur_dir}/functions/debug"
source "${cur_dir}/functions/have"
source "${cur_dir}/functions/is_func"
source "${cur_dir}/functions/pathappend"
source "${cur_dir}/functions/pathprepend"
source "${cur_dir}/functions/pathunmunge"
source "${cur_dir}/functions/sp"
source "${cur_dir}/functions/ssh_agents"

# autoload all of the functions
for func in ${cur_dir}/functions/*; do
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
unset func cur_dir
