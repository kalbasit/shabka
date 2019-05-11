{ lib, ... }:

with lib;

let
  coworkersExpr = /yl/private/network-secrets/shabka/hosts/demeter/coworkers.nix;

  coworkers = optionalAttrs (builtins.pathExists coworkersExpr) (import coworkersExpr);

  makeUser = userName: { uid, sshAuthorizedKeys, home ? "/home/${userName}" }: nameValuePair
    userName
    {
      inherit home uid;

      group = "mine";
      extraGroups = [
        "builders"
        "fuse"
        "users"
      ];

      isNormalUser = true;

      openssh.authorizedKeys.keys = singleton sshAuthorizedKeys;
    };

in {
  users = {
    users = (mapAttrs' makeUser coworkers);
  };
}
