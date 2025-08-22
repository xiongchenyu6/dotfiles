# SOPS Migration Guide - Separating Secrets from Configuration

This guide helps you migrate from storing mixed configuration and secrets in sops to a cleaner approach where only actual secrets are encrypted.

## Step 1: Backup Current Secrets

```bash
# Create a backup of current secrets
cp secrets/common.yaml secrets/common.yaml.backup
```

## Step 2: Edit Secrets File

You need to manually edit the sops file to restructure it. Run:

```bash
sops secrets/common.yaml
```

## Step 3: Make These Changes in the Editor

### For rust-web-server

**DELETE the entire `config` block** and replace with ONLY the secrets:

From:
```yaml
rust-web-server:
    config: |
        database:
          url: postgresql://rustwebserver:rustwebserver@localhost/rustwebserver
        oauth:
          client_id: vr-control
          client_secret: vVuC5jLwHe7y6wF8hdNLgtxJGru2PUk1PwGEVPBCV0wx6hFZ
          redirect_url: https://rust-server.auto-life.tech/auth/authorized
          config_url: https://kanidm.auto-life.tech/oauth2/openid/vr-control/.well-known/openid-configuration
        oidc:
          client_id: robot-management-system
          issuer: https://kanidm.auto-life.tech/oauth2/openid/robot-management-system
          authorization_endpoint: https://kanidm.auto-life.tech/oauth2/openid/robot-management-system
          token_endpoint: https://kanidm.auto-life.tech/oauth2/token
          userinfo_endpoint: https://kanidm.auto-life.tech/oauth2/openid/robot-management-system/userinfo
          jwks_uri: https://kanidm.auto-life.tech/oauth2/openid/robot-management-system/public_key.jwk
          revocation_url: https://kanidm.auto-life.tech/oauth2/token/revoke
          introspection_endpoint: https://kanidm.auto-life.tech/oauth2/token/introspect
        auth:
          enabled: false
```

To (ONLY SECRETS):
```yaml
rust-web-server:
    db-password: rustwebserver
    oauth-client-secret: vVuC5jLwHe7y6wF8hdNLgtxJGru2PUk1PwGEVPBCV0wx6hFZ
```

**All the configuration (URLs, client IDs, endpoints) is now in the Nix template!**

### For openldap

**DELETE both `passwordFile` and `credentials` blocks** and replace with ONLY the password:

From:
```yaml
openldap:
    passwordFile: |
        uid=kadmin,ou=services,dc=autolife-robotics,dc=tech#{HEX}61
        uid=kdc,ou=services,dc=autolife-robotics,dc=tech#{HEX}61
    credentials: |
        ldapUser: "cn=admin,dc=autolife-robotics,dc=tech"
        ldapPass: "1"
```

To (ONLY THE SECRET):
```yaml
openldap:
    admin-password: "1"
```

**The LDAP user configuration is now in the Nix template!**

### For restic

Replace:
```yaml
restic:
    pass: restic_I9n7G7G0ZpDWA3GOcJbIuwQCGvGUBkU5
    s3: |
        AWS_ACCESS_KEY_ID=ti636mH3q2ESMgog
        AWS_SECRET_ACCESS_KEY=hlgiC0VOF0MPW6A9xoTnIFFY8wg0DNie4Z4gMUcR
```

With:
```yaml
restic:
    pass: restic_I9n7G7G0ZpDWA3GOcJbIuwQCGvGUBkU5
    aws-access-key-id: ti636mH3q2ESMgog
    aws-secret-access-key: hlgiC0VOF0MPW6A9xoTnIFFY8wg0DNie4Z4gMUcR
```

## Step 4: Save and Exit

When you save and exit, sops will automatically re-encrypt the file with the proper structure.

## Step 5: Verify the Changes

```bash
# Verify the file can be decrypted
sops -d secrets/common.yaml | head -20

# Check that the structure is correct
sops -d secrets/common.yaml | yq '.rust-web-server'
sops -d secrets/common.yaml | yq '.openldap'
sops -d secrets/common.yaml | yq '.restic'
```

## Step 6: Test the Configuration

```bash
# Build the configuration for one host
nixos-rebuild build --flake .#netbird

# If successful, apply it
nixos-rebuild switch --flake .#netbird
```

## Why This Migration?

1. **Security**: Only actual secrets (passwords, keys) should be encrypted
2. **Maintainability**: Configuration should be visible and versionable in Nix files
3. **Transparency**: Clear separation between what's secret and what's not
4. **Flexibility**: Easier to change configuration without re-encrypting

## Module Changes Already Made

The following Nix modules have been updated to use templates:

1. **rust-web-server.nix**: Now uses `sops.templates` for config generation
2. **openldap.nix**: Uses templates and systemd service for password injection
3. **server/backup/default.nix**: Uses templates for AWS environment variables
4. **Host configurations**: Updated to reference templates instead of secrets

## Troubleshooting

If you encounter issues:

1. Restore from backup: `cp secrets/common.yaml.backup secrets/common.yaml`
2. Check sops configuration: `sops --verbose secrets/common.yaml`
3. Verify age/pgp keys are available
4. Check module syntax: `nix flake check`