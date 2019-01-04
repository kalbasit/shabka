# Why this importPinned? See scripts/nixos-rebuild.sh
{ importPinned ? true }:

let
  pinnedVersion = builtins.fromJSON (builtins.readFile ./nixpkgs-stable-version.json);
  pinned = builtins.fetchGit {
    inherit (pinnedVersion) url rev ref;
  };
in
  if importPinned then import pinned {} else pinned
