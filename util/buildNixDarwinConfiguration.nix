{ ... }:

{
  buildNixDarwinConfiguration = conf: (import <darwin> {
    configuration = conf;
  });
}
