self: super:

{
  nur = super.lib.recursiveUpdate
    (import super.shabka.external.nur.path { pkgs = super; })
    ({
      repos = {
        kalbasit = import super.shabka.external.kalbasit.nur.path { pkgs = super; };
        risson = import super.shabka.external.risson.nur.path { pkgs = super; };
      };
    });
}
