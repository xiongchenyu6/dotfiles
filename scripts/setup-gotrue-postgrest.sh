#!/usr/bin/env bash
# Sets up GoTrue + PostgREST integration on oracle-arm-001.

set -e

echo "🔧 Setting up GoTrue + PostgREST Integration"
echo "============================================="

# Check if running on oracle-arm-001
if [[ "$(hostname)" != "oracle-arm-001" ]]; then
    echo "❌ This script should only be run on oracle-arm-001"
    exit 1
fi

echo "📝 Step 1: Adding GoTrue secrets to SOPS"
echo "You need to add the following secrets to secrets/common.yaml:"
echo ""
echo "gotrue:"
echo "    jwt-secret: <same-as-postgrest-jwt-secret>"
echo "    database-url: postgresql://rustwebserver:<password>@localhost:5432/rustwebserver"
echo ""
echo "To add these secrets:"
echo "1. sops secrets/common.yaml"
echo "2. Add the gotrue section using the same JWT secret as postgrest"
echo "3. Use the same database connection string with rustwebserver credentials"
echo ""

# Check if secrets exist
if ! sops -d secrets/common.yaml | yq '.gotrue' > /dev/null 2>&1; then
    echo "❌ GoTrue secrets not found in secrets/common.yaml"
    echo "Please add the secrets before continuing."
    echo ""
    echo "Example commands:"
    echo "cd /home/freeman.xiong/dotfiles"
    echo "sops secrets/common.yaml"
    echo "# Add the gotrue section as shown above"
    exit 1
fi

echo "✅ GoTrue secrets found in SOPS"

echo ""
echo "📊 Step 2: Rebuilding NixOS configuration"
echo "This will apply the GoTrue module and restart services..."

# Build and switch configuration
nixos-rebuild switch --flake .#oracle-arm-001

echo "✅ NixOS configuration updated"

echo ""
echo "🗄️  Step 3: Verifying database setup"

# Check if PostgreSQL is running
if ! systemctl is-active --quiet postgresql.service; then
    echo "❌ PostgreSQL is not running"
    exit 1
fi

echo "✅ PostgreSQL is running"

# Check if GoTrue database initialization was successful
if ! sudo -u postgres psql -d rustwebserver -c "SELECT EXISTS (SELECT FROM pg_tables WHERE schemaname = 'auth' AND tablename = 'users');" | grep -q "t"; then
    echo "❌ GoTrue auth tables not found. Running manual initialization..."
    sudo systemctl restart gotrue-db-init.service
    sleep 5
    if ! sudo -u postgres psql -d rustwebserver -c "SELECT EXISTS (SELECT FROM pg_tables WHERE schemaname = 'auth' AND tablename = 'users');" | grep -q "t"; then
        echo "❌ Failed to initialize GoTrue tables"
        exit 1
    fi
fi

echo "✅ GoTrue database tables found"

echo ""
echo "🚀 Step 4: Verifying services"

# Check GoTrue service
if ! systemctl is-active --quiet gotrue-supabase.service; then
    echo "⚠️  GoTrue service is not running. Starting..."
    sudo systemctl restart gotrue-supabase.service
    sleep 5
fi

if systemctl is-active --quiet gotrue-supabase.service; then
    echo "✅ GoTrue service is running"
else
    echo "❌ GoTrue service failed to start"
    echo "Check logs: sudo journalctl -u gotrue-supabase.service -f"
    exit 1
fi

# Check PostgREST service
if systemctl is-active --quiet postgrest.service; then
    echo "✅ PostgREST service is running"
else
    echo "❌ PostgREST service is not running"
    exit 1
fi

# Check nginx
if systemctl is-active --quiet nginx.service; then
    echo "✅ Nginx service is running"
else
    echo "❌ Nginx service is not running"
    exit 1
fi

echo ""
echo "🧪 Step 5: Testing endpoints"

# Test GoTrue endpoint
if curl -s -f https://auth.corp.autolife.ai/health > /dev/null; then
    echo "✅ GoTrue endpoint is accessible"
else
    echo "⚠️  GoTrue endpoint may not be accessible (could be normal if health endpoint doesn't exist)"
fi

# Test PostgREST endpoint
if curl -s -f https://api.corp.autolife.ai > /dev/null; then
    echo "✅ PostgREST endpoint is accessible"
else
    echo "❌ PostgREST endpoint is not accessible"
fi

echo ""
echo "🎉 Setup Complete!"
echo "=================="
echo ""
echo "Your GoTrue + PostgREST integration is now configured:"
echo "• GoTrue Auth:   https://auth.corp.autolife.ai"
echo "• PostgREST API: https://api.corp.autolife.ai"
echo ""
echo "📚 Next steps:"
echo "1. Review the documentation: docs/supabase-postgrest-integration.md"
echo "2. Create your first user account through the API"
echo "3. Set up Row Level Security policies for your tables"
echo "4. Configure your frontend application"
echo ""
echo "🔍 Troubleshooting:"
echo "• GoTrue logs:    sudo journalctl -u gotrue-supabase.service -f"
echo "• PostgREST logs: sudo journalctl -u postgrest.service -f"
echo "• Nginx logs:     sudo journalctl -u nginx.service -f"
echo "• Database logs:  sudo journalctl -u postgresql.service -f"
echo ""
echo "🧪 Test user creation:"
echo 'curl -X POST https://auth.corp.autolife.ai/signup \'
echo '  -H "Content-Type: application/json" \'
echo '  -d '"'"'{"email": "test@example.com", "password": "password123"}'"'"
echo ""
