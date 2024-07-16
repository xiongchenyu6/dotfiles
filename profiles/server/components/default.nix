{ pkgs, ... }:
{
  imports = [
    ./backup
    ./security.nix
  ];
  environment = {
    systemPackages = with pkgs; [
      krb5Full
      python3
      eza
      calibre
      cyrus_sasl
      xvfb-run
      figlet
      #step-cli
    ];
  };

  programs = {
    rust-motd = {
      enable = false;
      settings = {
        banner = {
          color = "red";
          command = "${pkgs.nettools}/bin/hostname | ${pkgs.figlet}/bin/figlet -f slant";
        };
        uptime = {
          prefix = "Up";
        };
        last_run = { };
        service_status = {
          kdc = "kdc";
          kadmind = "kadmind";
          openldap = "openldap";
        };
        s_s_l_certs = {
          sort_method = "manual";
          certs = {
            "*.inner.autolife-robotics.me" = "/var/lib/acme/.autolife-robotics.me/cert.pem";
            "mail.autolife-robotics.me" = "/var/lib/acme/mail.autolife-robotics.me/cert.pem";
          };
        };
        # weather = { loc = "Singapore,Singapore"; };
        filesystems = {
          root = "/";
        };
        memory = {
          swap_pos = "beside";
        };
        last_login = {
          freeman = 2;
        };
      };
    };
  };
  systemd.services.rust-motd = {
    serviceConfig = {
      User = "acme";
    };
  };
}
