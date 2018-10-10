{
  services.openssh.enable = true;

  # Support for my workflow. This can be removed once SWM v2 lands.
  services.openssh.extraConfig = ''
    Match User kalbasit
      AcceptEnv ACTIVE_PROFILE
      AcceptEnv ACTIVE_STORY
  '';

  # allow Mosh server in
  networking.firewall.allowedUDPPortRanges = [ { from = 60000; to = 61000; } ];
}
