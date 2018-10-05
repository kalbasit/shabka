self: super:

{
  # timewarrior errors out if it can't write the config file, even though it's tracked by Nix
  # If timewarrior.cfg is not writable, timew errors out with Insufficient permissions for '/home/kalbasit/.timewarrior/timewarrior.cfg'.
  # I believe the root cause is https://github.com/GothenburgBitFactory/timewarrior/blob/004afd64a5556ba3d35fd99c14d82f9b3ca64f1b/src/init.cpp#L183-L186
  # TODO: file a ticket upstream, and try to fix it with an overlay
  timewarrior = super.timewarrior.overrideAttrs (oa: {
    patches = [./timewarrior-no-write-config-file.patch];
  });
}
