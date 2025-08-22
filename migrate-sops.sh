#!/usr/bin/env bash
set -e

echo "This script will help you migrate secrets/common.yaml to contain ONLY secrets"
echo "It will create a new file with the correct structure that you can then encrypt"
echo ""

# Backup current file
echo "Creating backup..."
cp secrets/common.yaml secrets/common.yaml.backup-$(date +%Y%m%d-%H%M%S)

# Create temporary decrypted file
echo "Decrypting current secrets..."
sops -d secrets/common.yaml > /tmp/secrets-decrypted.yaml

# Create new secrets file with ONLY secrets
echo "Creating new secrets structure..."
cat > /tmp/secrets-new.yaml << 'EOF'
postgrest:
    pass: ENC_PLACEHOLDER
    jwt-secret: ENC_PLACEHOLDER
nixAccessTokens: ENC_PLACEHOLDER
cloudflared:
    tunnel-credentials: ENC_PLACEHOLDER
ssh:
    freeman.xiong:
        id_ed25519: ENC_PLACEHOLDER
oath:
    seed: ENC_PLACEHOLDER
user:
    freeman:
        pass: ENC_PLACEHOLDER
    root:
        pass: ENC_PLACEHOLDER
falcon:
    cid: ENC_PLACEHOLDER
acme:
    cloudflare: ENC_PLACEHOLDER
    volcengine: ENC_PLACEHOLDER
wireguard:
    office: ENC_PLACEHOLDER
    tcloud: ENC_PLACEHOLDER
    game: ENC_PLACEHOLDER
    game-office: ENC_PLACEHOLDER
database:
    postgres:
        discourse: ENC_PLACEHOLDER
        gitea: ENC_PLACEHOLDER
        keycloak: ENC_PLACEHOLDER
cachix:
    binary-caches: ENC_PLACEHOLDER
    cachix-secret: ENC_PLACEHOLDER
# ONLY password, no config!
openldap:
    admin-password: "1"
datadog: ENC_PLACEHOLDER
# ONLY individual secrets, no s3 config block!
restic:
    pass: ENC_PLACEHOLDER
    aws-access-key-id: ENC_PLACEHOLDER
    aws-secret-access-key: ENC_PLACEHOLDER
netbird:
    coturn:
        password: ENC_PLACEHOLDER
# ONLY secrets, no config block!
rust-web-server:
    db-password: rustwebserver
    oauth-client-secret: vVuC5jLwHe7y6wF8hdNLgtxJGru2PUk1PwGEVPBCV0wx6hFZ
kanidm:
    vr-control: ENC_PLACEHOLDER
openfortivpn:
    server: ENC_PLACEHOLDER
    username: ENC_PLACEHOLDER
    password: ENC_PLACEHOLDER
    trusted_cert: ENC_PLACEHOLDER
    otp_code: ENC_PLACEHOLDER
api-keys:
    SILICON_FLOW: ENC_PLACEHOLDER
    XAI_API_KEY: ENC_PLACEHOLDER
    OPENROUTER_API_KEY: ENC_PLACEHOLDER
    GEMINI_API_KEY: ENC_PLACEHOLDER
    Github_Access_Token: ENC_PLACEHOLDER
    SLACK_BOT_TOKEN: ENC_PLACEHOLDER
    SLACK_TEAM_ID: ENC_PLACEHOLDER
    GITHUB_TOKEN: ENC_PLACEHOLDER
sing-box:
    V2RAY: ENC_PLACEHOLDER
dev-shell:
    KADMIN: ENC_PLACEHOLDER
    KDC: ENC_PLACEHOLDER
    TRUN_PASSWORD: ENC_PLACEHOLDER
    DataStoreEncryptionKey: ENC_PLACEHOLDER
    IDP_ClientSecret: ENC_PLACEHOLDER
    FRP: ENC_PLACEHOLDER
EOF

echo ""
echo "Now you need to manually edit the secrets file to remove configuration:"
echo ""
echo "1. Run: sops secrets/common.yaml"
echo ""
echo "2. Make these changes:"
echo "   - DELETE the entire 'rust-web-server.config' block"
echo "   - Keep only 'rust-web-server.db-password' and 'rust-web-server.oauth-client-secret'"
echo ""
echo "   - DELETE 'openldap.passwordFile' and 'openldap.credentials'"  
echo "   - Keep only 'openldap.admin-password'"
echo ""
echo "   - DELETE 'restic.s3' block"
echo "   - Keep only 'restic.pass', 'restic.aws-access-key-id', and 'restic.aws-secret-access-key'"
echo ""
echo "3. Save and exit"
echo ""
echo "The new structure is saved in /tmp/secrets-new.yaml for reference"