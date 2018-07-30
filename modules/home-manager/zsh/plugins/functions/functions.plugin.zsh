# load the important functions
source ./debug
source ./have
source ./is_func
source ./pathappend
source ./pathprepend
source ./pathunmunge
source ./sp
source ./ssh_agents

# autoload all of the functions
for func in *; do
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
