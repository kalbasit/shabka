# reload zshrc
function zshrc() {
  autoload -U compinit zrecompile
  compinit -d "${ZSH_COMPDUMP}"

  for f in ~/.zshrc "${ZSH_COMPDUMP}"; do
    zrecompile -p "${f}" && command rm -f "${f}.zwc.old"
  done

  source ~/.zshrc
}
