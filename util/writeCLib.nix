{ runCommandCC }:

{
  writeCLib = name: code:
    runCommandCC name
    {
      inherit name code;
      passAsFile = ["code"];
      # Pointless to do this on a remote machine.
      preferLocalBuild = true;
      allowSubstitutes = false;
    }
    ''
    n=$out/lib/$name
    mkdir -p "$(dirname "$n")"
    mv "$codePath" code.c
    $CC -x c code.c -o "$n"
    '';
}
