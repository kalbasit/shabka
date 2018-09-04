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

  # Patch kernel 4.18 to fix my mouse.
  # https://github.com/NixOS/nixpkgs/issues/45165
  linux_4_18 = super.linux_4_18.override {
    kernelPatches = super.linux_4_18.kernelPatches ++ [{
      name = "fix-kernel-#200847";
      patch = super.fetchpatch {
        url = "https://patchwork.kernel.org/patch/10587369/raw/";
        sha256 = "07z1cp3mkiwy7r8sqvzjafrk80p8xrza82zfx85whm3vgngi3bwp";
      };
    }];
  };
}
