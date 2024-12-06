{
  profiles,
  config,
  lib,
  ...
}:
{
  sops.secrets."oath/seed" = { };
  security = {
    audit = {
      enable = true;
      rules = [
        "-a exit,always -F arch=b64 -F euid=0 -S execve"
        "-a exit,always -F arch=b32 -F euid=0 -S execve"
      ];
    };
    auditd.enable = true;

    #rtkit = { enable = true; };
    sudo = {
      enable = true;
      execWheelOnly = lib.mkForce false;
      # wheelNeedsPassword = false;
      # package = pkgs.sudo.override {
      #   withInsults = true;
      #   withSssd = true;
      # };
    };

    acme = {
      acceptTerms = true;
    };
    polkit.enable = true;
    pam = {
      krb5.enable = false;
      oath = {
        enable = false;
        usersFile = config.sops.secrets."oath/seed".path;
        window = 30;
      };
    };
    pki = {
      certificates = map (x: x.cert) profiles.share.root-cas;
    };
  };
  services = {
    logrotate = {
      settings = {
        "/var/log/audit/*.log" = {
          frequency = "daily";
          rotate = 7;
          compress = true;
          #delaycompress = true;
          missingok = true;
          notifempty = true;
          postrotate = ''
            systemctl restart auditd
          '';
        };
      };
    };
  };
}
