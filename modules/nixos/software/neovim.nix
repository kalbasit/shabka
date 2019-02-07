{ pkgs, ... }:

let
  neovimConfig = import ../../neovim {
    # inherit (cfg) extraRC extraKnownPlugins extraPluginDictionaries keyboardLayout;
    inherit pkgs;
    keyboardLayout = "colemak";
  };

in {
  environment.systemPackages = with pkgs; [
    direnv

    (wrapNeovim neovim.unwrapped {
      inherit (neovimConfig)
          extraPython3Packages withPython3
          extraPythonPackages withPython
          withNodeJs withRuby viAlias vimAlias configure;
    })
  ];
}
