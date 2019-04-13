{ mkExternal }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);

  src = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [
    # https://github.com/rycee/home-manager/pull/474
    ./474-fix-switch-user-without-profile.patch
  ];

  patched = mkExternal {
    inherit src patches;

    name = "home-manager";
    revision = pinnedVersion.rev;
  };
in {
  path = patched;
}
