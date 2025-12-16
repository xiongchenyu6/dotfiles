{
  pkgs,
  config,
  lib,
  inputs,
  ...
}:
{
  # Import the gotrue-supabase module from your flake input
  imports = [
    inputs.xiongchenyu6.nixosModules.gotrue-supabase
  ];

  # Create environment files via SOPS templates
  sops.templates."gotrue-jwt-secret" = {
    content = ''
      GOTRUE_JWT_SECRET=${config.sops.placeholder."postgrest/jwt-secret"}
    '';
    mode = "0600";
    owner = "root";
    group = "root";
  };

  sops.templates."gotrue-database-url" = {
    content = ''
      DB_DATABASE_URL=postgresql://rustwebserver:${config.sops.placeholder."rust-web-server/db-password"}@localhost:5432/rustwebserver
      DATABASE_URL=postgresql://rustwebserver:${config.sops.placeholder."rust-web-server/db-password"}@localhost:5432/rustwebserver
    '';
    mode = "0600";
    owner = "root";
    group = "root";
  };

  services.gotrue-supabase = {
    enable = true;
    package = pkgs.gotrue-supabase;
    
    # Basic configuration
    siteUrl = "https://auth.${config.networking.domain}";
    apiExternalUrl = "https://auth.${config.networking.domain}";
    listenAddress = "127.0.0.1";
    apiPort = 8081;
    logLevel = "info";

    # Database connection - same as postgrest
    # databaseUrl will be set via environment file for security

    # Environment files for secrets
    environmentFiles = [
      config.sops.templates."gotrue-jwt-secret".path
      config.sops.templates."gotrue-database-url".path
    ];

    # Additional settings for Supabase Auth - use mkForce to resolve conflicts
    settings = lib.mkForce {
      # Enable email confirmations
      GOTRUE_MAILER_AUTOCONFIRM = false;
      GOTRUE_EXTERNAL_EMAIL_ENABLED = true;

      # JWT settings - must match postgrest
      GOTRUE_JWT_AUD = "authenticated";
      GOTRUE_JWT_EXP = 3600;

      # Enable anonymous sign-ins (optional)
      GOTRUE_EXTERNAL_ANONYMOUS_USERS_ENABLED = true;

      # Disable phone auth for simplicity (enable if needed)
      GOTRUE_EXTERNAL_PHONE_ENABLED = false;

      # Password requirements
      GOTRUE_PASSWORD_MIN_LENGTH = 8;

      # Rate limiting
      GOTRUE_RATE_LIMIT_HEADER = "X-Real-IP";

      # SMTP settings (to be configured via environment file if needed)
      GOTRUE_SMTP_ADMIN_EMAIL = "admin@${config.networking.domain}";
      GOTRUE_SMTP_HOST = "localhost";
      GOTRUE_SMTP_PORT = 587;
    };
  };

  # PostgreSQL setup for GoTrue - add necessary auth tables
  # This will run after PostgreSQL and PostgREST start but before GoTrue starts
  systemd.services.gotrue-supabase = {
    after = [
      "postgresql.service"
      "postgrest.service"
    ];
    wants = [
      "postgresql.service"
      "postgrest.service"
    ];
  };

  # Initialize GoTrue tables in PostgreSQL
  systemd.services.gotrue-db-init = {
    description = "Initialize GoTrue database tables";
    wantedBy = [ "gotrue-supabase.service" ];
    before = [ "gotrue-supabase.service" ];
    after = [
      "postgresql.service"
      "postgrest.service"
    ];
    wants = [
      "postgresql.service"
      "postgrest.service"
    ];

    script = ''
      # Wait for PostgreSQL to be ready
      while ! ${pkgs.postgresql_18_jit}/bin/psql -U freeman.xiong -d rustwebserver -c '\q' 2>/dev/null; do
        echo "Waiting for PostgreSQL..."
        sleep 2
      done

      # Create auth schema and tables if they don't exist
      ${pkgs.postgresql_18_jit}/bin/psql -U freeman.xiong -d rustwebserver << 'EOF'
      -- Create auth schema
      CREATE SCHEMA IF NOT EXISTS auth;

      -- Create users table for GoTrue
      CREATE TABLE IF NOT EXISTS auth.users (
        instance_id UUID,
        id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
        aud VARCHAR(255),
        role VARCHAR(255),
        email VARCHAR(255) UNIQUE,
        encrypted_password VARCHAR(255),
        email_confirmed_at TIMESTAMPTZ,
        invited_at TIMESTAMPTZ,
        confirmation_token VARCHAR(255),
        confirmation_sent_at TIMESTAMPTZ,
        recovery_token VARCHAR(255),
        recovery_sent_at TIMESTAMPTZ,
        email_change_token_new VARCHAR(255),
        email_change VARCHAR(255),
        email_change_sent_at TIMESTAMPTZ,
        last_sign_in_at TIMESTAMPTZ,
        raw_app_meta_data JSONB,
        raw_user_meta_data JSONB,
        is_super_admin BOOLEAN,
        created_at TIMESTAMPTZ DEFAULT NOW(),
        updated_at TIMESTAMPTZ DEFAULT NOW(),
        phone VARCHAR(255) UNIQUE,
        phone_confirmed_at TIMESTAMPTZ,
        phone_change VARCHAR(255),
        phone_change_token VARCHAR(255),
        phone_change_sent_at TIMESTAMPTZ,
        confirmed_at TIMESTAMPTZ GENERATED ALWAYS AS (LEAST(email_confirmed_at, phone_confirmed_at)) STORED,
        email_change_token_current VARCHAR(255),
        email_change_confirm_status SMALLINT,
        banned_until TIMESTAMPTZ,
        reauthentication_token VARCHAR(255),
        reauthentication_sent_at TIMESTAMPTZ,
        is_sso_user BOOLEAN DEFAULT FALSE,
        deleted_at TIMESTAMPTZ
      );

      -- Create refresh_tokens table
      CREATE TABLE IF NOT EXISTS auth.refresh_tokens (
        instance_id UUID,
        id BIGSERIAL PRIMARY KEY,
        token VARCHAR(255) UNIQUE,
        user_id UUID REFERENCES auth.users(id) ON DELETE CASCADE,
        revoked BOOLEAN,
        created_at TIMESTAMPTZ DEFAULT NOW(),
        updated_at TIMESTAMPTZ DEFAULT NOW(),
        parent VARCHAR(255),
        session_id UUID
      );

      -- Create instances table  
      CREATE TABLE IF NOT EXISTS auth.instances (
        id UUID PRIMARY KEY,
        uuid UUID,
        raw_base_config TEXT,
        created_at TIMESTAMPTZ DEFAULT NOW(),
        updated_at TIMESTAMPTZ DEFAULT NOW()
      );

      -- Create sessions table
      CREATE TABLE IF NOT EXISTS auth.sessions (
        id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
        user_id UUID REFERENCES auth.users(id) ON DELETE CASCADE,
        created_at TIMESTAMPTZ DEFAULT NOW(),
        updated_at TIMESTAMPTZ DEFAULT NOW(),
        factor_id UUID,
        aal TEXT,
        not_after TIMESTAMPTZ
      );

      -- Grant permissions to rustwebserver user
      GRANT USAGE ON SCHEMA auth TO rustwebserver;
      GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA auth TO rustwebserver;
      GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA auth TO rustwebserver;
      GRANT ALL PRIVILEGES ON ALL FUNCTIONS IN SCHEMA auth TO rustwebserver;

      -- Set default privileges for future objects
      ALTER DEFAULT PRIVILEGES IN SCHEMA auth GRANT ALL PRIVILEGES ON TABLES TO rustwebserver;
      ALTER DEFAULT PRIVILEGES IN SCHEMA auth GRANT ALL PRIVILEGES ON SEQUENCES TO rustwebserver;
      ALTER DEFAULT PRIVILEGES IN SCHEMA auth GRANT ALL PRIVILEGES ON FUNCTIONS TO rustwebserver;
      EOF
    '';

    serviceConfig = {
      Type = "oneshot";
      User = "postgres";
      Group = "postgres";
    };
  };

  # Firewall configuration
  networking.firewall.allowedTCPPorts = [
    8081 # GoTrue API port
  ];
}
