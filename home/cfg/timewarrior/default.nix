{ pkgs, ... }:

{
  home.packages = with pkgs; [
    timewarrior
  ];

  # If timewarrior.cfg is not writable, timew errors out with Insufficient permissions for '/home/kalbasit/.timewarrior/timewarrior.cfg'.
  # I believe the root cause is https://github.com/GothenburgBitFactory/timewarrior/blob/004afd64a5556ba3d35fd99c14d82f9b3ca64f1b/src/init.cpp#L183-L186
  # TODO: file a ticket upstream, and try to fix it with an overlay
  # home.file.".timewarrior/timewarrior.cfg".text = builtins.readFile (pkgs.substituteAll {
  #   src = ./timewarriorcfg;
  #
  #   timewarrior_out = "${pkgs.timewarrior}";
  # });
}
