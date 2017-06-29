#
# vim:ft=zsh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#

# vim_clean_swap goes through all of Vim's swap files and:
# - delete the swap file if it's the same as the original file
# - opens vim in diff mode between the swap file and the original file
function vim_clean_swap() {
  TMPDIR=$(mktemp -d vimrecovery.XXXXXXXX)
  RECTXT="$TMPDIR/vim.recovery.$USER.txt"
  RECFN="$TMPDIR/vim.recovery.$USER.fn"
  trap 'rm -f "$RECTXT" "$RECFN"; rmdir "$TMPDIR"' 0 1 2 3 15
  for q in ~/.vim/_swap/.*sw? ~/.vim/_swap/*; do
    [[ -f $q ]] || continue
    if [[ `du $q | awk '{print $1}'` -eq 0 ]]; then
      rm -f $q
      continue
    fi
    rm -f "$RECTXT" "$RECFN"
    vim -X -r "$q" \
      -c "w! $RECTXT" \
      -c "let fn=expand('%')" \
      -c "new $RECFN" \
      -c "exec setline( 1, fn )" \
      -c w\! \
      -c "qa"
    if [[ ! -f $RECFN ]]; then
      echo "nothing to recover from $q"
      rm -f "$q"
      continue
    fi
    CRNT="$(cat $RECFN)"
    if diff --strip-trailing-cr --brief "$CRNT" "$RECTXT"; then
      echo "removing redundant $q"
      echo "  for $CRNT"
      rm -f "$q"
    else
      echo "$q contains changes"
      if vim -n -d "$CRNT" "$RECTXT"; then
          rm -i "$q"
      fi
    fi
  done
}
