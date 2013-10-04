# Debian / Ubuntu sets these to vi-up-line-or-history etc,
# # which places the cursor at the start of line, not end of line.
# # See: http://www.zsh.org/mla/users/2009/msg00878.html
if [[ -f "/etc/lsb-release" ]]; then
  bindkey -M viins "\e[A" up-line-or-history
  bindkey -M viins "\e[B" down-line-or-history
  bindkey -M viins "\eOA" up-line-or-history
  bindkey -M viins "\eOB" down-line-or-history
fi
