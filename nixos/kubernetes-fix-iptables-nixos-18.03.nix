{ config, pkgs, ... }:

{
  # Kubernetes does not work on 18.03 due to
  # https://github.com/kubernetes/kubernetes/issues/58956 so apply the patch that fixed that issue
  nixpkgs.overlays = [(self: super: {
    kubernetes = super.kubernetes.overrideAttrs (oa: {
      patches = (oa.patches or []) ++ [
        (self.fetchpatch {
          # https://github.com/kubernetes/kubernetes/pull/60978
          # Fixes critical kube-proxy failure on iptables-restore >= 1.6.2 and
          # non-critical failures on prior versions.
          url = "https://github.com/kubernetes/kubernetes/commit/34ce573e9992ecdbc06dff1b4e3d0e9baa8353dd.patch";
          sha256 = "1sd9qgc28zr6fkk0441f89bw8kq2kadys0qs7bgivy9cmcpw5x5p";
        })];
      });
    })];
}
