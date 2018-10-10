with import <nixpkgs> {};

with lib;

let
  getDir =  (dir: mapAttrs (file: type:
    if type == "directory"
    then getDir "${dir}/${file}"
    else type)
    (builtins.readDir dir));

  dirFiles =
    dir: collect isString
    (mapAttrsRecursive
      (path: type: concatStringsSep "/" path)
      (getDir dir));

  allFiles =
    dir: collect isString
    (mapAttrsRecursive
      (path: type: concatStringsSep "/" path)
      (getDir dir));

  matchFile = regexp: file:
    null != (builtins.match regexp file);

  anyMatchForFile = regexps: file:
    builtins.all (regexp: !(matchFile regexp file)) regexps;

  filteredFiles = regexps: dir:
    map (file: dir + "/${file}")
      (filter (file: anyMatchForFile regexps file) (allFiles dir));

  filteredModules = regexps: dir:
    map (file: import file) (filteredFiles regexps dir);

  recImport = dir:
    map (file: dir + "/${file}")
      (filter (file:
        ((hasSuffix ".nix" file) && (!(hasSuffix ".lib.nix" file)) && (file != "default.nix")))
        (dirFiles dir));

  recCallPackage = dir:
    let content = builtins.readDir dir; in
      builtins.listToAttrs
        (map (n: {name = n; value = callPackage (dir + ("/" + n)) {}; })
        (builtins.filter (n: builtins.pathExists (dir + ("/" + n + "/default.nix")))
          (builtins.attrNames content)));

  utilPackages = map (m: callPackage (import m) {}) (recImport ./.);

in foldl' (x: y: x // y) {
  inherit getDir dirFiles allFiles matchFile anyMatchForFile
    filteredFiles filteredModules recImport recCallPackage;
} utilPackages
