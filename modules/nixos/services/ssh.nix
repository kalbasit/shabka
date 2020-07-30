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

  swmSupport = concatStringsSep
    "\n"
    (map
      (user: if (hasPrefix "yl" user) then ''
        Match User ${user}
          AcceptEnv ZSH_PROFILE
          AcceptEnv SWM_STORY_NAME
      '' else "")
      myUsers);

  extraSocketSupport = ''
    StreamLocalBindUnlink yes
  '';

in {
  services.openssh.enable = true;

  # Support for my workflow. This can be removed once SWM v2 lands.
  services.openssh.extraConfig = swmSupport + extraSocketSupport;

  # allow Mosh server in
  networking.firewall.allowedUDPPortRanges = [ { from = 60000; to = 61000; } ];
}
