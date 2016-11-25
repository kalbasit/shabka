#
# vim:ft=sh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

# devnfs() #{{{
function devnfs() {
  docker-machine ssh dev '\
  export IP="$(netstat -rn | grep eth1 | awk "{print $1}" | cut -d. -f1-3).1" \
  && sudo umount /Users \
  && sudo /usr/local/etc/init.d/nfs-client start >/dev/null \
  && sudo mount $IP:/Users /Users -o rw,async,noatime,rsize=32768,wsize=32768,proto=tcp \
  && echo "Mounted /Users over NFS"'
}
#}}}
