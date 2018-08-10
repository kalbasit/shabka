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
}
