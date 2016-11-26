#
# vim:ft=sh:fenc=UTF-8:ts=4:sts=4:sw=4:expandtab:foldmethod=marker:foldlevel=0:
#
# Some functions are taken from
#       http://phraktured.net/config/
#       http://www.downgra.de/dotfiles/

# sw() {{{
function sw() {
    local current_profile=
    local repo=
    # record the current profile
    current_profile="${ACTIVE_PROFILE}"
    # start personal
    sp kill
    tmx -n -d "${HOME}/.dotfiles"
    for repo in github.com/kalbasit/{advantage2,tf-k8s,tf-vpc,tf-etcd,terraform}; do
        tmx -n -d "$GOPATH/src/${repo}"
    done
    # start publica
    sp publica
    for repo in github.com/publica-project/{adctrl,bootstrap-js,console,console-api,demo,deploy-tools,emr,grpc-logger,homepage,js-app,js-utils,kube-manifests,pig,protobuf-defs,request-logger,storm-s3,terraform,videojs-publica-ads,website}; do
        tmx -n -d "$GOPATH/src/${repo}"
    done
    # start dailymotion
    sp kill
    for repo in github.com/dailymotion/{ad-controller,player}; do
        tmx -n -d "$GOPATH/src/${repo}"
    done
    # restore the current profile
    if [[ -n "${current_profile}" ]]; then
        sp "${current_profile}"
    fi
}
# }}}
