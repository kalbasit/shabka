{ self, lib, home-manager }:

{ modules ? [], system, ... }@args:
lib.nixosSystem {
  inherit system;
  modules = modules ++ [
    self.nixosModules.shabka
    {
      options.home-manager.users = lib.mkOption {
        type = lib.types.attrsOf (lib.types.submoduleWith {
          modules = self.nixosModules.shabkaHome;
        });
      };
    }
    home-manager.nixosModules.home-manager
  ];
  specialArgs = {
    shabka = self;
  };
}
