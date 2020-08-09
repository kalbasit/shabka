{ config, pkgs, lib, ... }:

# TODO: make it work with worktree
# See https://github.com/input-output-hk/iohk-nix/blob/master/commit-id.nix
# let
#   system_rev =
#     let
#       generateLabel = path: "${builtins.substring 0 7 (lib.commitIdFromGitRepo path)}";
#       readGitDir = path:
#         let
#           gitContents = lib.readFile path;
#         in
#           lib.tail (builtins.split " " gitContents);
#
#       gitDir = ../../.git;
#     in
#       generateLabel
#         (if lib.pathIsDirectory gitDir then gitDir else (builtins.toPath (readGitDir gitDir)));
#
#   label = "nixos_${config.system.nixos.version}-shabka_${system_rev}";
#
# in {
#   system.nixos.label = label;
# }

let
  shabka = import <shabka> { };

  iohk-nix = import shabka.external.iohk-nix.path {};

  git_dir = ../../../.git;

  # TODO: This is hardcoded! It should instead point to whatever value pkgs.path is
  pinnedNixpkgsVersion = builtins.fromJSON (builtins.readFile ../../../external/nixpkgs/20.03/version.json);

  label = "nixos_${config.system.nixos.release}-${builtins.substring 0 7 pinnedNixpkgsVersion.rev}-shabka_${builtins.substring 0 7 (iohk-nix.commitIdFromGitRepo git_dir)}";

in {
  # git is required to compute the label within lib.commitIdFromGitRepo.
  environment.systemPackages = [ pkgs.git ];

  # system.nixos.label = label;
}
