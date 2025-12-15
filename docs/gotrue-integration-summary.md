# GoTrue + PostgREST Integration Summary

## What was added

### 1. GoTrue-Supabase Module
- **File**: `/home/freeman.xiong/dotfiles/nixos-modules/gotrue-supabase.nix`
- **Purpose**: Configures Supabase Auth (GoTrue) service with PostgreSQL integration
- **Features**:
  - JWT authentication compatible with PostgREST
  - Automatic database table initialization
  - SOPS secrets integration
  - CORS-enabled nginx configuration

### 2. Oracle-ARM-001 Configuration Update
- **File**: `/home/freeman.xiong/dotfiles/nixos-configurations/oracle-arm-001/default.nix`
- **Changes**:
  - Added GoTrue module import
  - Added nginx virtual host for `auth.corp.autolife.ai`
  - CORS headers for frontend integration
  - SSL/TLS termination

### 3. Documentation
- **File**: `/home/freeman.xiong/dotfiles/docs/supabase-postgrest-integration.md`
- **Content**: Comprehensive guide for frontend developers including:
  - Authentication flow diagrams
  - JavaScript/React code examples
  - API usage patterns
  - Error handling strategies
  - Testing procedures

### 4. Setup Script
- **File**: `/home/freeman.xiong/dotfiles/scripts/setup-gotrue-postgrest.sh`
- **Purpose**: Automated setup and verification script
- **Features**:
  - SOPS secrets validation
  - Service health checks
  - Database initialization verification
  - Endpoint testing

## Architecture

```
Frontend Application
        ↓
┌─────────────────┐    JWT Token    ┌─────────────────┐
│   Supabase Auth │ ←─────────────→ │   PostgREST API │
│ auth.corp.autolife.ai │            │ api.corp.autolife.ai │
└─────────────────┘                 └─────────────────┘
        ↓                                   ↓
┌─────────────────────────────────────────────────────┐
│              PostgreSQL Database                     │
│  • auth.users (GoTrue tables)                      │
│  • your_app_tables (with RLS)                      │
└─────────────────────────────────────────────────────┘
```

## Key Configuration Points

### JWT Secret Sharing
Both GoTrue and PostgREST use the same JWT secret stored in SOPS:
- `gotrue/jwt-secret` (same value as `postgrest/jwt-secret`)

### Database Connection
Both services connect to the same PostgreSQL instance:
- Database: `rustwebserver`
- User: `rustwebserver` 
- Connection managed via SOPS secrets

### SSL/TLS
Both services are behind nginx with:
- SSL termination
- CORS headers for browser compatibility
- Automatic HTTPS redirect

## Required Manual Steps

### 1. Add SOPS Secrets
```bash
cd /home/freeman.xiong/dotfiles
sops secrets/common.yaml
```
Add:
```yaml
gotrue:
    jwt-secret: <same-value-as-postgrest-jwt-secret>
    database-url: postgresql://rustwebserver:<password>@localhost:5432/rustwebserver
```

### 2. Deploy Configuration
```bash
nixos-rebuild switch --flake .#oracle-arm-001
```

### 3. Run Setup Script
```bash
./scripts/setup-gotrue-postgrest.sh
```

## Service Endpoints

- **GoTrue Auth**: `https://auth.corp.autolife.ai`
- **PostgREST API**: `https://api.corp.autolife.ai`
- **Web Interface**: `https://rust-server.autolife.ai`

## Frontend Integration

### Basic Setup
```javascript
import { createClient } from '@supabase/supabase-js'

const supabase = createClient(
  'https://auth.corp.autolife.ai',
  'your-anon-key'
)

// Sign in
const { data, error } = await supabase.auth.signInWithPassword({
  email: 'user@example.com',
  password: 'password'
})

// Get token for API calls
const token = data.session.access_token

// Call PostgREST API
fetch('https://api.corp.autolife.ai/your_table', {
  headers: {
    'Authorization': `Bearer ${token}`
  }
})
```

## Security Features

### Row Level Security (RLS)
PostgreSQL RLS policies ensure users can only access their own data:
```sql
ALTER TABLE your_table ENABLE ROW LEVEL SECURITY;
CREATE POLICY "Users see own data" ON your_table
  FOR SELECT USING (auth.uid() = user_id);
```

### CORS Protection
Nginx configuration includes proper CORS headers for browser security.

### JWT Validation
PostgREST automatically validates JWT tokens and extracts user context.

## Monitoring and Logs

### Service Logs
```bash
# GoTrue service
sudo journalctl -u gotrue-supabase.service -f

# PostgREST service  
sudo journalctl -u postgrest.service -f

# Database initialization
sudo journalctl -u gotrue-db-init.service -f
```

### Health Checks
The setup script includes automated health checks for all components.

## Next Steps for Frontend Developers

1. Read the full documentation: `docs/supabase-postgrest-integration.md`
2. Set up your development environment with the provided examples
3. Create database tables with appropriate RLS policies
4. Test authentication flow with the provided JavaScript examples
5. Build your application using the PostgREST API patterns

## Support

For issues:
1. Check service logs using the commands above
2. Verify SOPS secrets are correctly configured
3. Ensure SSL certificates are valid
4. Test individual components using curl commands in the documentation