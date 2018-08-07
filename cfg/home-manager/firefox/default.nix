{ pkgs, ... }:

{
  home.file.".mozilla/firefox/profiles.ini".text = ''
    [General]
    StartWithLastProfile=1

    [Profile0]
    Name=personal
    IsRelative=1
    Path=profiles/personal

    [Profile1]
    Name=publica
    IsRelative=1
    Path=profiles/publica
  '';

  home.packages = with pkgs; [
    firefox
  ];
}
