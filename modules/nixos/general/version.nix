{ config, lib, ... }:

# TODO: make it work with worktree
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
#   label = "nixos_${config.system.nixos.version}-system_${system_rev}";
#
# in {
#   system.nixos.label = label;
# }

let
  git_dir = ../../.git;
  label = "nixos_${config.system.nixos.version}" + (if lib.pathIsDirectory git_dir then
    "-system_${builtins.substring 0 7 (lib.commitIdFromGitRepo git_dir)}"
  else "-story");

in {
  system.nixos.label = label;
}
