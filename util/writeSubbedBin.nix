{ stdenv, substituteAll }:

{
  writeSubbedBin = args:
    substituteAll (args // {
      dir = "bin";
      isExecutable = true;
      shell = stdenv.shell;
    });
}
