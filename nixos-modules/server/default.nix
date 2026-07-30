{ pkgs, lib, ... }:
{
  # ./backup removed 2026-07-30: it pushed restic to Tebi, which shut the whole
  # object-storage service down on 2026-03-31. Every run had been failing since
  # (invalid access key), so the hosts have no backup at all until a new
  # provider is wired up.
  imports = [
    ./security.nix
  ];
  environment = {
    systemPackages = with pkgs; [
      #krb5Full
      python3
      eza
      #calibre
      #cyrus_sasl
      figlet # for generating ASCII art
      #step-cli
      mtr
      ldns
      websocat
    ];
  };
  # systemd.services.datadog-agent.serviceConfig.User = lib.mkForce "root";
  # systemd.services.datadog-agent.serviceConfig.Group = lib.mkForce "root";

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
        #     "*.inner.${config.networking.domain}" = "/var/lib/acme/.${config.networking.domain}/cert.pem";
        #     "mail.${config.networking.domain}" = "/var/lib/acme/mail.${config.networking.domain}/cert.pem";
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
