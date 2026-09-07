# sex.qd.je — static ebook-library site (repo: ~/Dropbox/pua-books, built with build.py,
# rsynced into /var/www/sex.qd.je by its deploy.sh). The free host it started on
# rate-limited and suspended the domain, so nginx here serves it directly.
#
# The domain's DNS lives at DigitalPlat (a free qd.je subdomain that Cloudflare
# cannot host), so the certificate uses HTTP-01 instead of the host-wide
# Cloudflare DNS default. /api/ is proxied to the payment Worker (Cloudflare
# Workers + D1) that verifies crypto payments on-chain and records them.
{ config, lib, pkgs, ... }:
let
  root = "/var/www/sex.qd.je";
  workerHost = "pua-pay.xiongchenyu6.workers.dev";
in
{
  systemd.tmpfiles.rules = [ "d ${root} 0755 freeman.xiong users -" ];

  security.acme.certs."sex.qd.je" = {
    dnsProvider = lib.mkForce null;
    environmentFile = lib.mkForce null;
    webroot = lib.mkForce "/var/lib/acme/acme-challenge";
  };

  services.nginx.virtualHosts."sex.qd.je" = {
    forceSSL = true;
    enableACME = true;
    inherit root;
    extraConfig = ''
      charset utf-8;
      # the site's .htaccess rules, translated
      location = / {
        if ($http_accept_language ~* "^zh") { return 302 /zh/; }
        if ($http_accept_language ~* "^ja") { return 302 /ja/; }
        if ($http_accept_language ~* "^ko") { return 302 /ko/; }
        if ($http_accept_language ~* "^es") { return 302 /es/; }
        if ($http_accept_language ~* "^ru") { return 302 /ru/; }
        if ($http_accept_language ~* "^fr") { return 302 /fr/; }
        if ($http_accept_language ~* "^de") { return 302 /de/; }
        if ($http_accept_language ~* "^pt") { return 302 /pt/; }
        if ($http_accept_language ~* "^it") { return 302 /it/; }
        if ($http_accept_language ~* "^tr") { return 302 /tr/; }
        if ($http_accept_language ~* "^vi") { return 302 /vi/; }
        return 302 /en/;
      }
      error_page 404 /en/404.html;
    '';
    locations = {
      "/" = {
        tryFiles = "$uri $uri/ =404";
        extraConfig = ''
          location ~* \.html$ { add_header Cache-Control "max-age=300, must-revalidate"; }
          location ~* \.(css|js|svg|webp|jpg|mp4)$ { add_header Cache-Control "max-age=2592000, public"; }
          location ~* \.(pdf|epub)$ { add_header Content-Disposition attachment; }
        '';
      };
      "/api/" = {
        proxyPass = "https://${workerHost}/";
        extraConfig = ''
          proxy_ssl_server_name on;
          proxy_set_header Host ${workerHost};
          proxy_set_header X-Forwarded-For $remote_addr;
          proxy_set_header X-Real-IP $remote_addr;
        '';
      };
    };
  };
}
