{ ... }:
{
  imports = [
    ./kernel.nix
    ./security.nix
    ./ssh-harden.nix
    ./virtualisation.nix
  ];
  # services.avahi = {
  #   nssmdns4 = true;
  #   enable = true;
  #   ipv4 = true;
  #   ipv6 = true;
  #   allowPointToPoint = true;
  #   reflector = true;
  #   publish = {
  #     enable = true;
  #     addresses = true;
  #     workstation = true;
  #   };
  # };

  services.resolved = {
    enable = true;
    # dnssec = "allow-downgrade";
    # dnsovertls = "opportunistic";
    llmnr = "true";
  };
}
