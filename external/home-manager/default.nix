{ mkExternal }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./version.json);

  src = builtins.fetchTarball {
    inherit (pinnedVersion) url sha256;
  };

  patches = [
    # https://github.com/rycee/home-manager/pull/474
    ./474-fix-switch-user-without-profile.patch

    # https://github.com/rycee/home-manager/pull/583
    ./583-activate-hm-through-postactivation.patch

    # https://github.com/rycee/home-manager/pull/586
    ./586-install-packages-through-user-packages.patch

    # https://github.com/rycee/home-manager/pull/587
    ./587-login-as-user.patch
  ];

  patched = mkExternal {
    inherit src patches;

    name = "home-manager";
    revision = pinnedVersion.rev;
  };
in {
  path = patched;
}
