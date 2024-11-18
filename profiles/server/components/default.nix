{ pkgs, lib, ... }:
{
  imports = [
    ./backup
    ./security.nix
  ];
  environment = {
    systemPackages = with pkgs; [
      #krb5Full
      python3
      eza
      #calibre
      #cyrus_sasl
      xvfb-run # for running GUI applications in headless mode
      figlet # for generating ASCII art
      #step-cli
    ];
  };
  systemd.services.datadog-agent.serviceConfig.User = lib.mkForce "root";
  systemd.services.datadog-agent.serviceConfig.Group = lib.mkForce "root";

  programs = {
    rust-motd = {
      enable = true;
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
          nginx = "nginx";
        };
        # s_s_l_certs = {
        #   sort_method = "manual";
        #   certs = {
        #     "*.inner.autolife-robotics.me" = "/var/lib/acme/.autolife-robotics.me/cert.pem";
        #     "mail.autolife-robotics.me" = "/var/lib/acme/mail.autolife-robotics.me/cert.pem";
        #   };
        # };
        # weather = {
        #   loc = "Singapore, Singapore";
        #   args = "--ipv4";
        # };
        filesystems = {
          root = "/";
        };
        memory = {
          swap_pos = "beside";
        };
        last_login = {
          "freeman.xiong" = 2;
        };
        last_run = { };
      };
    };
  };
  # systemd.services.rust-motd = {
  #   serviceConfig = {
  #     User = "acme";
  #   };
  # };
}
