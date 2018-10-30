# TODO(high): This module must be gated by options, specially the mosh firewall one.

{ config, lib, ... }:

with lib;

let

  myUsers =
    filter
      (entry: if entry != "" then true else false)
      (map
        (user: if hasPrefix "yl" user then user else "")
        (builtins.attrNames config.users.users));

  sshExtraConfig = concatStringsSep
    "\n"
    (map
      (user: if (hasPrefix "yl" user) then ''
        Match User ${user}
          AcceptEnv ACTIVE_PROFILE
          AcceptEnv ACTIVE_STORY
      '' else "")
      myUsers);

in {
  services.openssh.enable = true;

  # Support for my workflow. This can be removed once SWM v2 lands.
  services.openssh.extraConfig = sshExtraConfig;

  # allow Mosh server in
  networking.firewall.allowedUDPPortRanges = [ { from = 60000; to = 61000; } ];
}
