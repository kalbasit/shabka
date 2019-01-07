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
          sha256 = "0pcg4na9nqkc7xhjs71d2bdaysh2r0ydf19vn4xqmnb9fmvr8gmp";
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
