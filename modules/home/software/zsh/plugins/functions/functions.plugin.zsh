functions_root="${${(%):-%x}:A:h}"

# load the important functions
source "${functions_root}/debug"
source "${functions_root}/have"
source "${functions_root}/is_func"
source "${functions_root}/pathappend"
source "${functions_root}/pathprepend"
source "${functions_root}/pathunmunge"
source "${functions_root}/sp"

# autoload all of the functions
for func in $functions_root/*; do
	local func_name="$(basename ${func})"
  case "${func_name}" in
    _*)          ;;

    debug)       ;;
    have)        ;;
    is_func)     ;;
    pathappend)  ;;
    pathprepend) ;;
    pathunmunge) ;;
    sp)          ;;

    *)
      autoload -U "${func_name}"
      ;;
  esac
done
unset func
