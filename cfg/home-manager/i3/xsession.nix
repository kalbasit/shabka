{ ... }:

{
  xsession.enable = true;

  xsession.initExtra = ''
    exec > ~/.xsession-errors 2>&1
  '';
}
