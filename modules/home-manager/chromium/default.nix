{ pkgs, ... }:

{
  home.file.".config/chromium/profiles/anya/.keep".text = "";
  home.file.".config/chromium/profiles/ihab/.keep".text = "";
  home.file.".config/chromium/profiles/nosecurity/.keep".text = "";
  home.file.".config/chromium/profiles/personal/.keep".text = "";
  home.file.".config/chromium/profiles/publica/.keep".text = "";
  home.file.".config/chromium/profiles/vanya/.keep".text = "";

  home.file.".config/chromium/profiles/nosecurity/.cmdline_args".text = ''
    --disable-web-security
  '';

  home.packages = with pkgs; [
    chromium
  ];
}
