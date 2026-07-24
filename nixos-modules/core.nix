# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ ... }:
{

  imports = [ ../shared-modules/core.nix ];

  # networking.domain is intentionally NOT defaulted here. It used to default to
  # the corp domain, which silently gave every personal host a corp identity —
  # including wildcard ACME certs for a domain they never served. Hosts that
  # need a domain must set it explicitly.

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [
      "zh_CN.UTF-8/UTF-8"
      "en_US.UTF-8/UTF-8"
    ];
  };

  nix = {
    daemonCPUSchedPolicy = "idle";
    daemonIOSchedClass = "idle";
    gc = {
      randomizedDelaySec = "1h";
    };
    settings = {
      auto-optimise-store = true;
    };
  };

  system = {
    stateVersion = "25.05";
  }; # Did you read the comment?
}
