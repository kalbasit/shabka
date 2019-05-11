{ lib, ... }:

with lib;

let
  # expected format:
  #   {
  #     # name
  #     github-username = {
  #       uid = nnnn;
  #       sshAuthorizedKeys = with builtins; readFile (fetchurl {
  #         url = https://github.com/github-username.keys;
  #         sha256 = "0000000000000000000000000000000000000000000000000000";
  #       });
  #     };
  #   }
  coworkersExpr = /yl/private/network-secrets/shabka/hosts/demeter/coworkers;

  coworkers = optionalAttrs (builtins.pathExists coworkersExpr) (import coworkersExpr);

  makeUser = userName: { uid, sshAuthorizedKeys, home ? "/home/${userName}" }: nameValuePair
    userName
    {
      inherit home uid;

      group = "users";
      extraGroups = [
        "builders"
        "fuse"
      ];

      isNormalUser = true;

      openssh.authorizedKeys.keys = singleton sshAuthorizedKeys;
    };

in {
  users = {
    users = (mapAttrs' makeUser coworkers);
  };
}
