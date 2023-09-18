# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, ... }: {
  environment = {
    systemPackages = with pkgs; [
      krb5Full
      python3
      eza
      calibre
      cyrus_sasl
      xvfb-run
      figlet
    ];
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
          openldap = "openldap";
        };
        s_s_l_certs = {
          sort_method = "manual";
          certs = {
            "*.inner.autolife-robotics.tech" =
              "/var/lib/acme/inner.autolife-robotics.tech/cert.pem";
            "mail.autolife-robotics.tech" =
              "/var/lib/acme/mail.autolife-robotics.tech/cert.pem";
          };
        };
        # weather = { loc = "Singapore,Singapore"; };
        filesystems = { root = "/"; };
        memory = { swap_pos = "beside"; };
        last_login = {
          freeman = 2;
          user3 = 1;
        };
      };
    };
  };
  systemd.services.rust-motd = { serviceConfig = { User = "acme"; }; };
}
