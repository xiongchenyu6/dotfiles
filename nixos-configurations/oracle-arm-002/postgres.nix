{
  inputs,
  config,
  pkgs,
  lib,
  ...
}:

{
  imports = [
    inputs.xiongchenyu6.nixosModules.gotrue-supabase
    inputs.xiongchenyu6.nixosModules.supabase-realtime
  ];

  sops.secrets."oracle-arm-002/db-service-password" = { };
  sops.secrets."oracle-arm-002/jwt-secret" = { };
  sops.secrets."oracle-arm-002/quant-password" = { };
  sops.secrets."oracle-arm-002/realtime-secret-key-base" = { };
  sops.secrets."oracle-arm-002/realtime-metrics-jwt-secret" = { };

  sops.templates."postgrest-pgpass" = {
    content = ''
      *:*:*:*:${config.sops.placeholder."oracle-arm-002/db-service-password"}
    '';
    mode = "0400";
  };

  sops.templates."postgrest-jwt-secret" = {
    content = config.sops.placeholder."oracle-arm-002/jwt-secret";
    mode = "0400";
  };

  sops.templates."gotrue-env" = {
    content = ''
      GOTRUE_JWT_SECRET=${config.sops.placeholder."oracle-arm-002/jwt-secret"}
      DATABASE_URL=postgres://supabase_auth_admin:${config.sops.placeholder."oracle-arm-002/db-service-password"}@127.0.0.1:5432/api?search_path=auth&sslmode=disable
    '';
    mode = "0400";
  };

  # supabase-realtime runs as its own user, so the env file must be readable
  # by that user. The sops-nix default owner is root:root/0400, which the
  # service can't read — scope it to the realtime user.
  #
  # Also carries a couple of non-secret settings whose values contain
  # whitespace (DB_AFTER_CONNECT_QUERY, ERL_AFLAGS). `systemd.Environment=`
  # splits on spaces and rejects quoted-value shortcuts from Nix, so those
  # have to live in EnvironmentFile= where each line is parsed as KEY=value.
  sops.templates."realtime-env" = {
    content = ''
      DB_PASSWORD=${config.sops.placeholder."oracle-arm-002/db-service-password"}
      SECRET_KEY_BASE=${config.sops.placeholder."oracle-arm-002/realtime-secret-key-base"}
      API_JWT_SECRET=${config.sops.placeholder."oracle-arm-002/jwt-secret"}
      METRICS_JWT_SECRET=${config.sops.placeholder."oracle-arm-002/realtime-metrics-jwt-secret"}
      DB_AFTER_CONNECT_QUERY=SET search_path TO _realtime
      ERL_AFLAGS=-proto_dist inet_tcp
      DNS_NODES=
      RELEASE_COOKIE=supabase-realtime-single-node-cookie
    '';
    mode = "0400";
    owner = "supabase-realtime";
    group = "supabase-realtime";
  };

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_18_jit;
    enableJIT = true;
    enableTCPIP = true;
    authentication = ''
      local    all all                   trust
      host     all all 127.0.0.1/32      scram-sha-256
      host     all all ::1/128           scram-sha-256
      hostssl  api quant 0.0.0.0/0       scram-sha-256
      hostssl  api quant ::/0            scram-sha-256
    '';
    # Aligned with supabase/postgres: shared_preload_libraries from their
    # postgresql.conf.j2 minus the two extensions not yet in nixpkgs
    # (plan_filter, supabase_vault). All other preload-eligible extensions
    # are included; extensions that only need CREATE EXTENSION (pg_graphql,
    # pgjwt, pgtap, pgvector/pgvectorscale, pgmq, pg_repack, pg_hint_plan,
    # pgsql-http, wal2json, rum, postgis, plv8, pg_safeupdate, hstore,
    # citext, ltree, pgcrypto, uuid-ossp, btree_gin, btree_gist, etc.) are
    # installed via `extensions` below and `CREATE EXTENSION` in the init
    # script.
    extensions =
      ps: with ps; [
        # preload-heavy / shared_preload_libraries members
        # pgaudit — broken in nixpkgs for PG18 (2026-04). Re-add once the
        # upstream bump lands; Supabase uses it for audit logging but it's
        # not load-bearing for PostgREST/realtime/gotrue.
        plpgsql_check
        pg_cron
        pg_net
        pgsodium
        timescaledb
        pg_tle
        # on-demand extensions (created in init script below)
        pg_graphql
        pgjwt
        pgtap
        pgvector
        pgvectorscale
        pgmq
        pg_repack
        pg_hint_plan
        pgsql-http
        pg_safeupdate
        wal2json
        rum
        postgis
        plv8
        # Packaged locally in xiongchenyu6/nur-packages — extend PG's
        # extension set via the xiongchenyu6 overlay.
        pg_plan_filter
        pg_hashids
        index_advisor
        hypopg # dependency of index_advisor
      ];

    settings = {
      # auto_explain, pg_stat_statements, plpgsql are built into postgres —
      # they load from shared_preload_libraries without needing an extensions
      # entry. Order matters for timescaledb (must be last).
      shared_preload_libraries = lib.concatStringsSep "," [
        "pg_stat_statements"
        "plpgsql"
        "plpgsql_check"
        "pg_cron"
        "pg_net"
        "pgsodium"
        "auto_explain"
        "pg_tle"
        # pg_plan_filter is used exclusively via shared_preload_libraries
        # (no CREATE EXTENSION); GUCs like `plan_filter.statement_cost_limit`
        # then gate query plans that exceed a cost threshold.
        "plan_filter"
        "timescaledb"
      ];

      "cron.database_name" = "api";
      "cron.use_background_workers" = "on";
      max_worker_processes = 20;

      # Logical replication for supabase-realtime (wal2json + publication).
      # Tuned loosely to Supabase defaults.
      wal_level = "logical";
      max_wal_senders = 10;
      max_replication_slots = 10;

      log_connections = true;
      log_disconnections = true;
      log_destination = lib.mkForce "syslog";
      ssl = "on";
      ssl_cert_file = "/var/lib/postgresql-ssl/server.crt";
      ssl_key_file = "/var/lib/postgresql-ssl/server.key";
    };
    ensureDatabases = [ "api" ];
    ensureUsers = [
      {
        name = "freeman.xiong";
        ensureClauses.superuser = true;
      }
      { name = "api_authenticator"; }
      # Supabase-standard role names. The Realtime server's migrations and
      # most @supabase/* JS clients hardcode `anon` / `authenticated` /
      # `service_role`, so we match those names here rather than prefixing
      # with `api_`.
      { name = "anon"; }
      { name = "authenticated"; }
      { name = "service_role"; }
      { name = "supabase_auth_admin"; }
      {
        name = "supabase_admin";
        ensureClauses = {
          superuser = true;
          replication = true;
          createdb = true;
          createrole = true;
          bypassrls = true;
        };
      }
      { name = "quant"; }
    ];
  };

  systemd.services.postgres-ssl-cert = {
    description = "Generate self-signed TLS cert for PostgreSQL";
    before = [ "postgresql.service" ];
    wantedBy = [ "postgresql.service" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    script = ''
      set -euo pipefail
      DIR=/var/lib/postgresql-ssl
      EXPECTED_CN="db.panda.qzz.io"
      mkdir -p "$DIR"
      # Regenerate if the cert is missing or its CN no longer matches the
      # configured hostname (e.g. after a DNS rename).
      if [ -f "$DIR/server.crt" ]; then
        CURRENT_CN=$(${pkgs.openssl}/bin/openssl x509 -in "$DIR/server.crt" -noout -subject \
          | sed -n 's/.*CN[[:space:]]*=[[:space:]]*//p')
        if [ "$CURRENT_CN" != "$EXPECTED_CN" ]; then
          rm -f "$DIR/server.crt" "$DIR/server.key"
        fi
      fi
      if [ ! -f "$DIR/server.key" ] || [ ! -f "$DIR/server.crt" ]; then
        ${pkgs.openssl}/bin/openssl req -new -x509 -days 3650 -nodes \
          -out "$DIR/server.crt" -keyout "$DIR/server.key" \
          -subj "/CN=$EXPECTED_CN"
      fi
      chown -R postgres:postgres "$DIR"
      chmod 0700 "$DIR"
      chmod 0600 "$DIR/server.key"
      chmod 0644 "$DIR/server.crt"
    '';
  };

  systemd.services.postgresql-api-init = {
    description = "Provision api db: roles, schemas, extensions, auth helpers";
    # ensureDatabases/ensureUsers run in postgresql-setup.service (not
    # postgresql.service). postgresql.target pulls in both, so wait on it
    # to guarantee the `api` database and system roles exist before psql runs.
    after = [ "postgresql.target" ];
    requires = [ "postgresql.target" ];
    wantedBy = [ "multi-user.target" ];
    before = [
      "postgrest.service"
      "gotrue-supabase.service"
    ];

    serviceConfig = {
      Type = "oneshot";
      User = "postgres";
      Group = "postgres";
      RemainAfterExit = true;
      LoadCredential = [
        "db-pass:${config.sops.secrets."oracle-arm-002/db-service-password".path}"
        "quant-pass:${config.sops.secrets."oracle-arm-002/quant-password".path}"
      ];
    };

    script = ''
      set -euo pipefail
      PW=$(cat "$CREDENTIALS_DIRECTORY/db-pass")
      QPW=$(cat "$CREDENTIALS_DIRECTORY/quant-pass")
      export PW QPW
      ${pkgs.postgresql_18_jit}/bin/psql -v ON_ERROR_STOP=1 -d api <<SQL
        ALTER DATABASE api OWNER TO "freeman.xiong";

        -- Supabase-style default search_path: put `extensions` in scope so
        -- unqualified calls like hypopg_get_indexdef() (used internally by
        -- index_advisor) resolve without per-role ALTER.
        ALTER DATABASE api SET search_path TO "\$user", public, extensions;

        -- One-time migration from our old role name (api_anon) to
        -- Supabase's conventional `anon`. By the time this init script
        -- runs, postgresql-setup has already created the new `anon` role
        -- (ensureUsers), so we can't use RENAME — instead drop the old
        -- role after releasing its grants. The grants it used to hold
        -- (schema USAGE, membership in api_authenticator) are re-granted
        -- to `anon` further down in this same script, so nothing is lost.
        -- Idempotent: noop once api_anon is gone.
        DO \$fix\$
        BEGIN
          IF EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'api_anon') THEN
            REASSIGN OWNED BY api_anon TO anon;
            DROP OWNED BY api_anon CASCADE;
            DROP ROLE api_anon;
          END IF;
        END
        \$fix\$;

        ALTER ROLE api_authenticator   WITH LOGIN PASSWORD \$pw\$$PW\$pw\$;
        ALTER ROLE supabase_auth_admin WITH LOGIN PASSWORD \$pw\$$PW\$pw\$ CREATEROLE;
        ALTER ROLE supabase_admin      WITH LOGIN PASSWORD \$pw\$$PW\$pw\$;
        ALTER ROLE anon                WITH NOLOGIN;
        ALTER ROLE authenticated       WITH NOLOGIN;
        ALTER ROLE service_role        WITH NOLOGIN;
        ALTER ROLE quant               WITH LOGIN PASSWORD \$pw\$$QPW\$pw\$;

        GRANT anon, authenticated, service_role TO api_authenticator;

        CREATE SCHEMA IF NOT EXISTS api AUTHORIZATION "freeman.xiong";
        GRANT USAGE ON SCHEMA api TO anon, authenticated, service_role;

        CREATE SCHEMA IF NOT EXISTS auth AUTHORIZATION supabase_auth_admin;

        -- supabase-realtime's DB_AFTER_CONNECT_QUERY sets search_path to
        -- _realtime on every connection, including the very first one used
        -- to run migrations. Create the schema up front so that connect
        -- query doesn't fail before realtime's own Ecto migrator can run.
        CREATE SCHEMA IF NOT EXISTS _realtime AUTHORIZATION supabase_admin;

        -- Per-tenant realtime migrations expect a `realtime` schema for
        -- their schema_migrations + ephemeral tables. Create it up front
        -- so the first WS connect to a new tenant doesn't fail with
        -- `invalid_schema_name`.
        CREATE SCHEMA IF NOT EXISTS realtime AUTHORIZATION supabase_admin;

        CREATE SCHEMA IF NOT EXISTS quant AUTHORIZATION quant;
        GRANT CONNECT ON DATABASE api TO quant;
        ALTER ROLE quant SET search_path TO quant, public;

        -- Supabase-style namespaced extensions. Keeping heavy/stateful
        -- extensions (timescaledb, pg_cron) in their default schemas where
        -- their documentation assumes them; put the smaller helpers into
        -- a dedicated `extensions` schema exposed to the API roles.
        CREATE SCHEMA IF NOT EXISTS extensions AUTHORIZATION "freeman.xiong";
        GRANT USAGE ON SCHEMA extensions TO anon, authenticated, service_role;

        -- contrib grab-bag (install first — pgjwt etc. depend on pgcrypto)
        CREATE EXTENSION IF NOT EXISTS "uuid-ossp"         SCHEMA extensions;
        CREATE EXTENSION IF NOT EXISTS pgcrypto            SCHEMA extensions;
        CREATE EXTENSION IF NOT EXISTS hstore              SCHEMA extensions;
        CREATE EXTENSION IF NOT EXISTS citext              SCHEMA extensions;
        CREATE EXTENSION IF NOT EXISTS ltree               SCHEMA extensions;
        CREATE EXTENSION IF NOT EXISTS btree_gin           SCHEMA extensions;
        CREATE EXTENSION IF NOT EXISTS btree_gist          SCHEMA extensions;

        CREATE EXTENSION IF NOT EXISTS timescaledb;
        CREATE EXTENSION IF NOT EXISTS pg_cron;
        CREATE EXTENSION IF NOT EXISTS pg_stat_statements  SCHEMA extensions;
        CREATE EXTENSION IF NOT EXISTS plpgsql_check       SCHEMA extensions;
        CREATE EXTENSION IF NOT EXISTS pg_net              SCHEMA extensions;
        CREATE EXTENSION IF NOT EXISTS pgsodium;
        CREATE EXTENSION IF NOT EXISTS pg_tle;
        CREATE EXTENSION IF NOT EXISTS pg_graphql;
        CREATE EXTENSION IF NOT EXISTS pgjwt               SCHEMA extensions;
        CREATE EXTENSION IF NOT EXISTS pgtap               SCHEMA extensions;
        CREATE EXTENSION IF NOT EXISTS vector              SCHEMA extensions;
        CREATE EXTENSION IF NOT EXISTS vectorscale         SCHEMA extensions CASCADE;
        CREATE EXTENSION IF NOT EXISTS pgmq;
        CREATE EXTENSION IF NOT EXISTS pg_repack;
        CREATE EXTENSION IF NOT EXISTS pg_hint_plan;
        CREATE EXTENSION IF NOT EXISTS http                SCHEMA extensions;
        -- wal2json is a logical-decoding output plugin, not a CREATE
        -- EXTENSION target. It's kept in services.postgresql.extensions
        -- so the shared library is on postgres's dynamic_library_path and
        -- slots can specify `output_plugin => 'wal2json'`.
        CREATE EXTENSION IF NOT EXISTS rum                 SCHEMA extensions;
        CREATE EXTENSION IF NOT EXISTS postgis             SCHEMA extensions;
        CREATE EXTENSION IF NOT EXISTS plv8;
        CREATE EXTENSION IF NOT EXISTS pg_hashids           SCHEMA extensions;
        CREATE EXTENSION IF NOT EXISTS hypopg               SCHEMA extensions;
        CREATE EXTENSION IF NOT EXISTS index_advisor        SCHEMA extensions CASCADE;

        -- Logical replication publication consumed by supabase-realtime.
        -- Start empty — users add tables via
        -- `ALTER PUBLICATION supabase_realtime ADD TABLE ...`.
        DO \$\$
        BEGIN
          IF NOT EXISTS (SELECT 1 FROM pg_publication WHERE pubname = 'supabase_realtime') THEN
            CREATE PUBLICATION supabase_realtime;
          END IF;
        END
        \$\$;
      SQL
    '';
  };

  # gotrue owns auth.{users,identities,...} and the auth.{jwt,uid,role,email}
  # helper functions. Run `auth migrate` before gotrue-supabase.service so the
  # schema is in place on first boot and after upstream migrations are added.
  # Also grants EXECUTE on the helper functions to the PostgREST roles —
  # gotrue creates them, we just expose them.
  systemd.services.gotrue-migrate = {
    description = "Apply GoTrue database migrations";
    after = [ "postgresql-api-init.service" ];
    requires = [ "postgresql-api-init.service" ];
    before = [ "gotrue-supabase.service" ];
    wantedBy = [ "gotrue-supabase.service" ];

    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };

    script = ''
      set -euo pipefail
      EF=${config.sops.templates."gotrue-env".path}
      DB_URL=$(${pkgs.gnugrep}/bin/grep -E '^DATABASE_URL=' "$EF" | ${pkgs.coreutils}/bin/cut -d= -f2-)
      JWT_SECRET=$(${pkgs.gnugrep}/bin/grep -E '^GOTRUE_JWT_SECRET=' "$EF" | ${pkgs.coreutils}/bin/cut -d= -f2-)
      ${pkgs.coreutils}/bin/env \
        DATABASE_URL="$DB_URL" \
        GOTRUE_DB_DRIVER=postgres \
        GOTRUE_JWT_SECRET="$JWT_SECRET" \
        API_EXTERNAL_URL=https://auth.panda.qzz.io \
        GOTRUE_SITE_URL=https://auth.panda.qzz.io \
        ${pkgs.gotrue-supabase}/bin/auth migrate

      # Helper functions are owned by supabase_auth_admin after migrate. Grant
      # EXECUTE so PostgREST roles can call them from policies/views.
      ${pkgs.sudo}/bin/sudo -u postgres ${pkgs.postgresql_18_jit}/bin/psql \
        -v ON_ERROR_STOP=1 -d api <<'SQL'
        GRANT EXECUTE ON FUNCTION auth.jwt(), auth.uid(), auth.role(), auth.email()
          TO anon, authenticated, service_role;
      SQL
    '';
  };

  systemd.services.postgrest = {
    after = [ "postgresql-api-init.service" ];
    requires = [ "postgresql-api-init.service" ];
  };
  systemd.services.gotrue-supabase = {
    after = [
      "postgresql-api-init.service"
      "gotrue-migrate.service"
    ];
    requires = [
      "postgresql-api-init.service"
      "gotrue-migrate.service"
    ];
  };

  services.postgrest = {
    enable = true;
    pgpassFile = config.sops.templates."postgrest-pgpass".path;
    jwtSecretFile = config.sops.templates."postgrest-jwt-secret".path;
    settings = {
      db-uri = {
        host = "127.0.0.1";
        port = "5432";
        user = "api_authenticator";
        dbname = "api";
      };
      db-schemas = "api";
      db-anon-role = "anon";
      server-port = 3333;
      server-unix-socket = null;
      jwt-role-claim-key = ".role";
      jwt-aud = "authenticated";
      openapi-server-proxy-uri = "https://api.panda.qzz.io";
    };
  };

  services.gotrue-supabase = {
    enable = true;
    listenAddress = "127.0.0.1";
    # Module exports `apiPort` as API_PORT, but supabase/auth 2.x ignores that
    # env key and binds the default 8081 anyway. Leave apiPort unset (=8081)
    # and align nginx below to what the binary actually listens on.
    siteUrl = "https://auth.panda.qzz.io";
    apiExternalUrl = "https://auth.panda.qzz.io";
    environmentFiles = [ config.sops.templates."gotrue-env".path ];
    settings = {
      GOTRUE_JWT_AUD = "authenticated";
      GOTRUE_JWT_EXP = 3600;
      GOTRUE_JWT_DEFAULT_GROUP_NAME = "authenticated";
      GOTRUE_MAILER_AUTOCONFIRM = true;
      GOTRUE_EXTERNAL_EMAIL_ENABLED = true;
      GOTRUE_EXTERNAL_PHONE_ENABLED = false;
      GOTRUE_DISABLE_SIGNUP = false;
      GOTRUE_PASSWORD_MIN_LENGTH = 8;
      DB_NAMESPACE = "auth";
    };
  };

  services.supabase-realtime = {
    enable = true;
    port = 4000;
    database = {
      host = "127.0.0.1";
      port = 5432;
      user = "supabase_admin";
      name = "api";
    };
    environmentFiles = [ config.sops.templates."realtime-env".path ];
    settings = {
      APP_NAME = "realtime";
      RLIMIT_NOFILE = "10000";
      RUN_JANITOR = "true";
      SEED_SELF_HOST = "true";
      # Symmetric key used to encrypt per-tenant JWT secrets stored in
      # _realtime.tenants. Matches supabase/realtime docker-compose; stable
      # across restarts. ERL_AFLAGS / DNS_NODES / DB_AFTER_CONNECT_QUERY /
      # RELEASE_COOKIE live in the EnvironmentFile to avoid systemd's
      # whitespace-splitting on Environment= lines.
      DB_ENC_KEY = "supabaserealtime";
    };
  };
  # supabase-realtime needs our DB ready + roles provisioned.
  systemd.services.supabase-realtime = {
    after = [
      "postgresql.target"
      "postgresql-api-init.service"
    ];
    requires = [
      "postgresql.target"
      "postgresql-api-init.service"
    ];
  };

  services.postgresqlBackup = {
    enable = true;
    databases = [ "api" ];
  };

  networking.firewall.allowedTCPPorts = [ 5432 ];

  # DNS-01 wildcard cert for per-tenant subdomain routing. Issuance path
  # is Cloudflare (from security.acme.defaults.dnsProvider). nginx is
  # notified via reloadServices so it picks up new certs without a
  # manual `systemctl reload`.
  security.acme.certs."realtime.panda.qzz.io" = {
    domain = "realtime.panda.qzz.io";
    extraDomainNames = [ "*.realtime.panda.qzz.io" ];
    group = "nginx";
    reloadServices = [ "nginx.service" ];
  };

  services.nginx.virtualHosts = {
    "api.panda.qzz.io" = {
      forceSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:3333";
        proxyWebsockets = true;
      };
    };
    "auth.panda.qzz.io" = {
      forceSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass = "http://127.0.0.1:8081";
        proxyWebsockets = true;
        extraConfig = ''
          add_header 'Access-Control-Allow-Origin'      '*' always;
          add_header 'Access-Control-Allow-Methods'     'GET, POST, PUT, PATCH, DELETE, OPTIONS' always;
          add_header 'Access-Control-Allow-Headers'     'Authorization, Content-Type, X-Client-Info, apikey' always;
          if ($request_method = 'OPTIONS') { return 204; }
        '';
      };
    };
    "realtime.panda.qzz.io" = {
      forceSSL = true;
      # Wildcards require DNS-01, which `enableACME = true` (HTTP-01 via
      # webroot) can't do. Use a dedicated ACME cert declared below with
      # the cloudflare dnsProvider inherited from security.acme.defaults.
      useACMEHost = "realtime.panda.qzz.io";
      # Supabase Realtime routes tenants via Host header — the first label
      # becomes the tenant's external_id (e.g. smoke.realtime.panda.qzz.io
      # dispatches to tenant "smoke"). serverAliases pulls wildcard
      # requests into this vhost; the shared cert (below) covers them.
      serverAliases = [ "*.realtime.panda.qzz.io" ];
      locations."/" = {
        proxyPass = "http://127.0.0.1:4000";
        proxyWebsockets = true;
        extraConfig = ''
          proxy_read_timeout 3600s;
          proxy_send_timeout 3600s;
          # Host=$host is already set by nixpkgs' recommendedProxySettings,
          # so realtime's tenant resolver sees the original subdomain
          # without an extra proxy_set_header (which previously put us
          # over nginx's default proxy_headers_hash_bucket_size and made
          # the vhost 400 on every request).
        '';
      };
    };
  };

  # Bump proxy headers hash so all our vhosts' proxy_set_header entries
  # fit without triggering "could not build optimal proxy_headers_hash".
  services.nginx.proxyResolveWhileRunning = true;
  services.nginx.commonHttpConfig = lib.mkAfter ''
    proxy_headers_hash_max_size 1024;
    proxy_headers_hash_bucket_size 128;
  '';
}
