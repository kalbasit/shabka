self: super:

let
  # pkgs is a function given a path will create a set {name = callPackage name}
  pkgs = path:
  let content = builtins.readDir path; in
    builtins.listToAttrs
      (map (n: {name = n; value = super.callPackage (path + ("/" + n)) {}; })
      (builtins.filter (n: builtins.pathExists (path + ("/" + n + "/default.nix")))
        (builtins.attrNames content)));

  myPkgs = pkgs ../pkgs;
in
myPkgs // {
  # other overlay code goes here

  # timewarrior errors out if it can't write the config file, even though it's tracked by Nix
  # If timewarrior.cfg is not writable, timew errors out with Insufficient permissions for '/home/kalbasit/.timewarrior/timewarrior.cfg'.
  # I believe the root cause is https://github.com/GothenburgBitFactory/timewarrior/blob/004afd64a5556ba3d35fd99c14d82f9b3ca64f1b/src/init.cpp#L183-L186
  # TODO: file a ticket upstream, and try to fix it with an overlay
  timewarrior = super.timewarrior.overrideAttrs (oa: {
    patches = [./timewarrior-no-write-config-file.patch];
  });
}
