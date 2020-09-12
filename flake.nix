
{
  description = "shabka";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    home-manager.url = "github:rycee/home-manager";
    nur.url = "github:nix-community/NUR";
  };

  outputs = inputs@{ self, nixpkgs, home-manager, nur }:
  {
    nixosModules = {
      shabka = import ./modules/nixos-modules.nix;
      shabkaHome = import ./modules/home-modules.nix;
    };

    lib.nixosSystem = import ./lib/nixos-system.nix {
      inherit (nixpkgs) lib;
      inherit self home-manager;
    };

  };
  /*
    let
      inherit (builtins) attrNames attrValues readDir;
      inherit (nixpkgs) lib;
      inherit (lib) removeSuffix recursiveUpdate genAttrs filterAttrs;
      inherit (utils) pathsToImportedAttrs;

      system = "x86_64-linux";

      pkgImport = pkgs:
        import pkgs {
          inherit system;
          overlays = attrValues self.overlays;
          config = { allowUnfree = true; };
        };

      pkgs = pkgImport nixpkgs;

      # shabka NixOS modules
      moduleList = import ./modules/nixos/list.nix;
      # shabka home modules
      homeList = import ./modules/home/list.nix;

    in
    {
      devShell."${system}" = (import ./shell.nix { inherit pkgs; });

      overlay = import ./pkgs;

      overlays =
        let
          overlayDir = ./overlays;
          fullPath = name: overlayDir + "/${name}";
          overlayPaths = map fullPath (attrNames (readDir overlayDir));
        in
        pathsToImportedAttrs overlayPaths;

      packages."${system}" =
        let
          packages = self.overlay pkgs pkgs;
          overlays = lib.filterAttrs (n: v: n != "pkgs") self.overlays;
          overlayPkgs =
            genAttrs
              (attrNames overlays)
              (name: (overlays."${name}" pkgs pkgs)."${name}");
        in
        recursiveUpdate packages overlayPkgs;

      nixosModules = {
        # overrides
        overrides = {
          nixpkgs.overlays =
            let
              override = import "${self}/pkgs/override.nix" pkgs;

              overlay = pkg: final: prev: {
                "${pkg.pname}" = pkg;
              };
            in
            (map overlay override) ++ [ nur.overlay ];
        };

        # shabka NixOS modules
        shabkaModules = {
          _module.args.specialArgs = { inherit inputs; };
          imports = moduleList;
        };

        # home-manager
        inherit (home.nixosModules) home-manager;

        # shabka home modules
        shabkaHomeModules = {
          options.home-manager.users = lib.mkOption {
            type = lib.types.attrsOf (lib.types.submoduleWith {
              modules = homeList;
              specialArgs = { inherit inputs; };
            });
          };
        };
      };

      # shabka home modules, for access in home-manager only configuration
      homeModules = [
        {
          imports = homeList;
        }
      ];
    };
    */
}
