{ lib, ... }:

let
  git-hash = lib.commitIdFromGitRepo <system-path/.git>;

in {
  system.nixos.label = "github.com-kalbasit-system.${builtins.substring 0 7 git-hash}";
}
