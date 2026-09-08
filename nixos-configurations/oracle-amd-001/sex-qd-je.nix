# sex.qd.je — old name of the ebook-library site (repo: ~/Dropbox/pua-books), now a
# 301 to https://boob.qzz.io where the whole site runs on Cloudflare. The DNS for
# sex.qd.je lives at DigitalPlat (a free qd.je subdomain Cloudflare cannot host),
# hence the HTTP-01 / ZeroSSL certificate override below.
{ config, lib, pkgs, ... }:
{
  # Let's Encrypt counts qd.je as one registered domain shared by every free
  # subdomain, so its 50/week limit is permanently exhausted; ZeroSSL (EAB
  # credentials in sops, lego reads LEGO_EAB_* from the environment file) instead.
  sops.secrets."acme/zerossl" = { owner = "acme"; group = "acme"; mode = "0440"; };
  security.acme.certs."sex.qd.je" = {
    dnsProvider = lib.mkForce null;
    webroot = lib.mkForce "/var/lib/acme/acme-challenge";
    server = "https://acme.zerossl.com/v2/DV90";
    email = "zhihuiguo24@gmail.com";
    environmentFile = lib.mkForce config.sops.secrets."acme/zerossl".path;
    extraLegoFlags = [ "--eab" ];
  };

  # The site itself now lives on Cloudflare (Worker + static assets at boob.qzz.io);
  # this host only keeps the old name alive as a permanent redirect.
  services.nginx.virtualHosts."sex.qd.je" = {
    forceSSL = true;
    enableACME = true;
    locations."/".return = "301 https://boob.qzz.io$request_uri";
  };
}
