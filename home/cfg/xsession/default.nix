{ ... }:

{
  xsession.enable = true;

  xsession.initExtra = ''
    exec > ~/.xsession-errors 2>&1

    # fix the look of Java applications
    export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
  '';
}
