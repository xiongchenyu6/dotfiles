{ config, pkgs, options, lib, ... }:
let
  dbSuffix = "dc=freeman,dc=engineer";
  owner = "hydra";
in {
  sops.secrets."openldap/password" = {
    mode = "770";
    inherit owner;
    group = owner;
  };

  services = {
    hydra = {
      enable = true;
      package = pkgs.hydra-unstable;
      hydraURL =
        "https://hydra.inner.${config.networking.domain}"; # externally visible URL
      notificationSender =
        "${owner}@${config.networking.fqdn}"; # e-mail of hydra service
      dbi =
        "dbi:Pg:dbname=hydra;host=postgres-database-1.postgres.database.azure.com;user=hydra;password=hydra";
      # a standalone hydra will require you to unset the buildMachinesFiles list to avoid using a nonexistant /etc/nix/machines
      buildMachinesFiles = [ ];
      # you will probably also want, otherwise *everything* will be built from scratch
      useSubstitutes = true;
      # listenHost = "hydra.inner.${config.networking.domain}";
      extraConfig = ''
        <dynamicruncommand>
          enable = 1
        </dynamicruncommand>

        <hydra_notify>
          <prometheus>
            listen_address = 127.0.0.1
            port = 9199
          </prometheus>
        </hydra_notify>

        <ldap>
          <role_mapping>
            grafana = admin
            grafana = bump-to-front
            grafana = cancel-build
            grafana = create-projects
            grafana = restart-jobs
          </role_mapping>
          <config>
            <credential>
              class = Password
              password_field = password
              password_type = self_check
            </credential>
            <store>
              class = LDAP
              ldap_server = ldaps://${config.networking.fqdn}
              <ldap_server_options>
                timeout = 30
                debug = 2
              </ldap_server_options>
              binddn = "cn=admin,${dbSuffix}"
              include ${config.sops.secrets."openldap/password".path}
              start_tls = 0
              <start_tls_options>
                verify = none
              </start_tls_options>
              user_basedn = "ou=developers,${dbSuffix}"
              user_filter = "(&(objectClass=inetOrgPerson)(uid=%s))"
              user_scope = one
              user_field = uid
              <user_search_options>
                deref = always
              </user_search_options>
              # Important for role mappings to work:
              use_roles = 1
              role_basedn = "cn=grafana,ou=developers,${dbSuffix}"
              role_filter = "(&(objectClass=groupOfURLs)(member=%s))"
              role_field = cn
              role_value = dn
              <role_search_options>
                deref = always
              </role_search_options>
          </config>
        </ldap>
        </ldap>
      '';
    };
  };
}
