{ config, pkgs, ... }:          # multi-glibc-locale-paths.nix

/*
 * Provide version-specific LOCALE_ARCHIVE environment variables to mitigate
 * the effects of https://github.com/NixOS/nixpkgs/issues/38991.
 */

let

  # A random Nixpkgs revision *before* the default glibc
  # was switched to version 2.27.x.
  oldpkgsSrc = pkgs.fetchFromGitHub {
    owner = "nixos";
    repo = "nixpkgs";
    rev = "0252e6ca31c98182e841df494e6c9c4fb022c676";
    sha256 = "1sr5a11sb26rgs1hmlwv5bxynw2pl5w4h5ic0qv3p2ppcpmxwykz";
  };

  oldpkgs = import oldpkgsSrc {};

  # A random Nixpkgs revision *after* the default glibc
  # was switched to version 2.27.x.
  newpkgsSrc = pkgs.fetchFromGitHub {
    owner = "nixos";
    repo = "nixpkgs";
    rev = "1d0a71879dac0226272212df7a2463d8eeb8f75b";
    sha256 = "0nh6wfw50lx6wkzyiscfqg6fl6rb17wmncj8jsdvbgmsd6rm95rg";
  };

  newpkgs = import newpkgsSrc {};

in

{
  environment.sessionVariables = {
    LOCALE_ARCHIVE_2_21 = "${oldpkgs.glibcLocales}/lib/locale/locale-archive";
    LOCALE_ARCHIVE_2_27 = "${newpkgs.glibcLocales}/lib/locale/locale-archive";
  };
}
