# make sure DOTZSH is defined
if [[ -z "${DOTZSH}" ]]; then
	export DOTZSH="${0:a:h}"
fi

# add the zsh functions to the fpath
fpath+="${DOTZSH}/functions"
fpath+="${DOTZSH}/completions"

# load the important functions
source "${DOTZSH}/functions/debug"
source "${DOTZSH}/functions/have"
source "${DOTZSH}/functions/is_func"
source "${DOTZSH}/functions/pathappend"
source "${DOTZSH}/functions/pathprepend"
source "${DOTZSH}/functions/pathunmunge"
source "${DOTZSH}/functions/sp"
source "${DOTZSH}/functions/ssh_agents"

# autoload all of the functions
for func in ${DOTZSH}/functions/*; do
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
