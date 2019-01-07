{ assertMsg
, pkgs
}:

with pkgs;
with pkgs.lib;

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./home-manager-version.json);
  pinned = builtins.fetchGit {
    inherit (pinnedVersion) url rev;
  };

  importPinned = import pinned {};

  mkAssertMsg = name: "${name} is available upsteam, kill this patch";

  patches = [
    # https://github.com/rycee/home-manager/pull/529
    (
      let
        importPinned.programs.autorandr.profiles = {
          "default" = { config = { eDP1 = {}; }; };
        };
      in
        assert assertMsg (! importPinned.programs.autorandr.profiles."default".config.eDP1 ? transform) (mkAssertMsg "transform");
        fetchpatch {
          url = "https://github.com/rycee/home-manager/pull/529.patch";
          sha256 = "0k7d2za7hzsgqx77flzdkkadal0l3awzf047i521rzszkldc138r";
        }
    )
  ];

  patched = runCommand "home-manager-${pinnedVersion.rev}"
    {
      inherit pinned patches;

      preferLocalBuild = true;
    }
    ''
      cp -r $pinned $out
      chmod -R +w $out
      for p in $patches; do
        echo "Applying patch $p";
        patch -d $out -p1 < "$p";
      done
    '';
in
  patched
