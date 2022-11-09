# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }: {
  sops.secrets."oci-arm-host-capacity" = { };

  environment = {
    systemPackages = with pkgs; [ python3 calibre xvfb-run figlet ];
  };

  programs = {
    rust-motd = {
      enable = true;
      settings = {
        banner = {
          color = "red";
          command =
            "${pkgs.nettools}/bin/hostname | ${pkgs.figlet}/bin/figlet -f slant";
        };
        uptime = { prefix = "Up"; };
        last_run = { };
        service_status = {
          kdc = "kdc";
          kadmind = "kadmind";
          openldap = "slapd";
        };
        ssl_certificates = { sort_method = "manual"; };
        "ssl_certificates.certs" = {
          inner = "/var/lib/acme/inner.freeman.engineer/cert.pem";
          mail = "/var/lib/acme/mail.freeman.engineer/cert.pem";
        };
        weather = { loc = "New York,New York"; };
        filesystems = { root = "/"; };
        memory = { swap_pos = "beside"; };
        last_login = {
          freeman = 2;
          user3 = 1;
        };
      };
    };
  };

  services = {
    oci-arm-host-capacity = {
      enable = true;
      envPath = config.sops.secrets."oci-arm-host-capacity".path;
    };
  };
}
