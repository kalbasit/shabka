# add the zsh functions to the fpath
fpath=("${HOME}/.zsh/functions" $fpath)

# load the important functions
source "${HOME}/.zsh/functions/have"
source "${HOME}/.zsh/functions/sp"

# autoload all of the functions
for func in ${HOME}/.zsh/functions/*; do
  case "$(basename ${func})" in
    sp|have)
      ;;
    *)
      autoload -U "$(basename ${func})"
      ;;
  esac
done
unset func
