# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  services.endlessh-go = {
    enable = true;
    port = 22;
    prometheus = {
      enable = true;
      port = 9119;
    };
    openFirewall = true;
    extraOptions = [ "-geoip_supplier=ip-api" ];
  };
}
