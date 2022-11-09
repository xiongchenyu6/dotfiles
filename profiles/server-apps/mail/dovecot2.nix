{ config, pkgs, ... }:

{
  services = let
    credsDir = config.security.acme.certs."${config.networking.fqdn}".directory;
  in {
    dovecot2 = {
      enable = true;
      # group = "openldap";
      sslCACert = credsDir + "/full.pem";
      sslServerKey = credsDir + "/key.pem";
      sslServerCert = credsDir + "/cert.pem";
      enableImap = true;
      enablePop3 = true;
      enablePAM = true;
      enableQuota = true;
      enableLmtp = true;
      showPAMFailure = true;
      modules = [ pkgs.dovecot_pigeonhole ];
      mailboxes = {
        Drafts = {
          specialUse = "Drafts";
          auto = "subscribe";
        };
        Sent = {
          specialUse = "Sent";
          auto = "subscribe";
        };
        Trash = {
          specialUse = "Trash";
          auto = "subscribe";
        };
        Junk = {
          specialUse = "Junk";
          auto = "subscribe";
        };
        Archive = {
          specialUse = "Archive";
          auto = "subscribe";
        };
      };
      extraConfig = ''
        mail_debug = yes
        auth_debug = yes
        verbose_ssl = yes
        ssl = required
        ssl_min_protocol = TLSv1.2
        ssl_prefer_server_ciphers = yes
      '';
    };
  };
}
