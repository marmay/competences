# Deployment Guide

This guide covers deploying the Competences tracking application to a NixOS server in production.

## Architecture

**Deployment model:**
- Build frontend and backend on powerful PC
- Deploy to weak server using `nixos-rebuild --target-host`
- One WASM frontend build shared by all instances
- Multiple backend instances (one per class)
- Nginx reverse proxy with automatic HTTPS (Let's Encrypt)
- PostgreSQL databases (one per class)
- Secrets managed via agenix

**Directory structure:**
```
competences/
├── flake.nix                      # Main flake with nixosModules output
├── nix/
│   ├── backend.nix                # Backend package derivation
│   ├── frontend.nix               # WASM frontend package (optimized)
│   └── module.nix                 # NixOS module definition
└── docs/
    └── example-nixos-config.nix   # Complete example configuration
```

## Flake Outputs

**Packages** (per-system):
- `competences-backend`: Haskell backend executable (built with haskell.nix)
- `competences-frontend`: Pre-built WASM frontend (optimized with wasm-opt, stripped with wasm-tools)
- `default`: Combined package (convenience)

**NixOS Module**:
- `nixosModules.competences`: Service module for deployment

## Frontend Build Workflow

**Important:** The frontend WASM files are checked into git for reproducible deployments.

**Why:** Building WASM in Nix sandbox requires resolving Hackage dependencies, which needs network access. Until haskell.nix supports WASM cross-compilation, we commit pre-built WASM files.

**Workflow:**
1. Make frontend changes
2. Build locally: `./deploy_frontend.sh`
3. Commit updated files: `git add static/app.wasm static/ghc_wasm_jsffi.js && git commit`
4. Deploy as normal

**Files tracked in git:**
- `static/app.wasm` (~4.5MB, optimized and stripped)
- `static/ghc_wasm_jsffi.js` (~55KB, GHC WASM FFI runtime)
- `static/index.js` (~1KB, loader script)

This approach ensures:
- Reproducible builds (same WASM for all deployments)
- No network access needed during Nix build
- Fast CI/CD (no WASM compilation in pipeline)

**Future:** When haskell.nix supports WASM, we'll switch to building in Nix like the backend.

## Quick Start

### 1. Server Flake Configuration

Create or update your server's `flake.nix`:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    competences.url = "github:youruser/competences";
    # Or local path during development:
    # competences.url = "path:/path/to/competences";
    agenix.url = "github:ryantm/agenix";
  };

  outputs = { self, nixpkgs, competences, agenix }: {
    nixosConfigurations.yourserver = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        agenix.nixosModules.default
        competences.nixosModules.competences
        ./configuration.nix
      ];
    };
  };
}
```

### 2. NixOS Configuration

In your `configuration.nix`:

```nix
{ config, pkgs, ... }:
{
  services.competences = {
    enable = true;

    instances = {
      class-9a = {
        port = 8081;
        subdomain = "9a";
        database = "competences_class_9a";
        secretsFile = config.age.secrets.competences-9a.path;
        initDocument = ./data/class_9a.json;
      };

      class-9b = {
        port = 8082;
        subdomain = "9b";
        database = "competences_class_9b";
        secretsFile = config.age.secrets.competences-9b.path;
        initDocument = ./data/class_9b.json;
      };

      class-9c = {
        port = 8083;
        subdomain = "9c";
        database = "competences_class_9c";
        secretsFile = config.age.secrets.competences-9c.path;
        initDocument = ./data/class_9c.json;
      };

      class-9d = {
        port = 8084;
        subdomain = "9d";
        database = "competences_class_9d";
        secretsFile = config.age.secrets.competences-9d.path;
        initDocument = ./data/class_9d.json;
      };
    };

    nginx = {
      enable = true;
      domain = "competences.example.com";
      enableACME = true;  # Let's Encrypt
      forceSSL = true;
    };

    postgresql = {
      enable = true;
      # package = pkgs.postgresql_15;  # Optional: specify version
    };
  };

  # Configure agenix secrets (see Secrets Management section below)
  age.secrets = {
    competences-9a.file = ./secrets/9a-config.json.age;
    competences-9b.file = ./secrets/9b-config.json.age;
    competences-9c.file = ./secrets/9c-config.json.age;
    competences-9d.file = ./secrets/9d-config.json.age;
  };

  # Open firewall ports for Nginx
  networking.firewall.allowedTCPPorts = [ 80 443 ];
}
```

### 3. Deploy from Your PC

Build on your powerful PC and deploy to the server:

```bash
sudo nixos-rebuild switch \
  --flake .#yourserver \
  --target-host root@yourserver \
  --build-host localhost
```

This command:
- Builds all packages on your local machine (localhost)
- Copies the built packages to the server
- Activates the new configuration on the server

## Module Options

### services.competences

**Global options:**
- `enable` (bool): Enable the Competences service
- `package` (package): Backend package (default: auto-detected from flake)
- `frontendPackage` (package): Frontend package (default: auto-detected from flake)
- `staticDir` (path): Where to deploy frontend files (default: `/var/lib/competences/static`)

### services.competences.instances.<name>

**Per-instance options:**
- `port` (port, required): Port for this instance to listen on
- `subdomain` (string, required): Subdomain like "9a" for `9a.competences.example.com`
- `database` (string, required): PostgreSQL database name
- `secretsFile` (path, required): Path to config JSON with OAuth2 + JWT secrets
- `initDocument` (path, optional): Path to initial document JSON (used only if DB empty)
- `user` (string): System user for this instance (default: `competences-<name>`)
- `group` (string): System group (default: `competences`)

### services.competences.nginx

**Nginx reverse proxy options:**
- `enable` (bool): Enable nginx reverse proxy configuration
- `domain` (string): Base domain (e.g., "competences.example.com")
- `enableACME` (bool): Enable Let's Encrypt (default: true)
- `forceSSL` (bool): Force HTTPS redirects (default: true)

### services.competences.postgresql

**PostgreSQL options:**
- `enable` (bool): Auto-create databases for all instances
- `package` (package): PostgreSQL version to use (default: pkgs.postgresql)

## Secrets Management with agenix

### Secret File Format

Each instance needs a configuration file with this structure:

```json
{
  "jwtSecret": "your-secret-key-minimum-32-characters",
  "oauth2": {
    "clientId": "azure-client-id",
    "clientSecret": "azure-client-secret",
    "redirectUri": "https://9a.competences.example.com/oauth/callback",
    "tenantId": "azure-tenant-id"
  }
}
```

**Important:** The `redirectUri` must match the instance's FQDN.

### Setting Up Secrets

**1. Create plain config files:**
```bash
mkdir -p secrets
cat > secrets/9a-config.json <<EOF
{
  "jwtSecret": "$(openssl rand -base64 48)",
  "oauth2": {
    "clientId": "your-azure-client-id",
    "clientSecret": "your-azure-client-secret",
    "redirectUri": "https://9a.competences.example.com/oauth/callback",
    "tenantId": "your-azure-tenant-id"
  }
}
EOF
```

**2. Encrypt with agenix:**
```bash
# Ensure agenix is set up with your SSH keys in secrets.nix
agenix -e secrets/9a-config.json.age

# Delete the plaintext file
rm secrets/9a-config.json
```

**3. Configure in NixOS:**
```nix
age.secrets.competences-9a = {
  file = ./secrets/9a-config.json.age;
  # Optional: specify owner/group
  # owner = "competences-class-9a";
  # group = "competences";
};

services.competences.instances.class-9a.secretsFile =
  config.age.secrets.competences-9a.path;
```

**4. Commit encrypted files:**
```bash
git add secrets/*.age
git commit -m "Add encrypted config files"
```

At runtime, agenix automatically decrypts secrets to a temporary path that only the service can read.

## Generated Services

For each instance, the module automatically creates:

### Systemd Service

**Service name:** `competences-<name>.service`

**Properties:**
- Runs backend with correct CLI arguments
- Automatic restart on failure (10s delay)
- Waits for PostgreSQL if enabled
- Security hardening:
  - `NoNewPrivileges=true`
  - `PrivateTmp=true`
  - `ProtectSystem=strict`
  - `ProtectHome=true`

**Example:**
```bash
systemctl status competences-class-9a.service
systemctl restart competences-class-9a.service
journalctl -u competences-class-9a.service -f
```

### Nginx Virtual Host

**Domain:** `<subdomain>.<domain>`

**Features:**
- Reverse proxy to backend port
- WebSocket support (`proxyWebsockets = true`)
- HTTPS with Let's Encrypt (if enabled)
- Proper proxy headers (Host, X-Real-IP, X-Forwarded-For, X-Forwarded-Proto)
- HTTP → HTTPS redirect (if forceSSL enabled)

## Deployment Workflow

### Initial Deployment

```bash
# 1. Set up your server's flake.nix and configuration.nix
# 2. Create and encrypt secrets with agenix
# 3. Prepare initial data JSON files (e.g., data/class_9a.json)
# 4. Deploy:

sudo nixos-rebuild switch \
  --flake .#yourserver \
  --target-host root@yourserver \
  --build-host localhost
```

### Updating the Application

**After making changes to the competences repository:**

```bash
# On your server config repo:
# 1. Update the competences flake input
nix flake lock --update-input competences

# 2. Deploy
sudo nixos-rebuild switch \
  --flake .#yourserver \
  --target-host root@yourserver \
  --build-host localhost
```

**Updating configuration only (no code changes):**

```bash
# Just rebuild without updating inputs
sudo nixos-rebuild switch \
  --flake .#yourserver \
  --target-host root@yourserver \
  --build-host localhost
```

### Rolling Back

```bash
# On the server:
sudo nixos-rebuild switch --rollback

# Or list generations and switch to specific one:
nix-env --list-generations --profile /nix/var/nix/profiles/system
sudo /nix/var/nix/profiles/system-<N>-link/bin/switch-to-configuration switch
```

## Database Management

### Automatic Setup

If `services.competences.postgresql.enable = true`, databases are automatically created.

### Manual Setup

If you prefer manual database management:

```bash
# Create databases
sudo -u postgres createdb competences_class_9a
sudo -u postgres createdb competences_class_9b
# ... etc

# Initialize schema (first time only)
sudo -u postgres psql -d competences_class_9a < /path/to/backend/schema.sql
sudo -u postgres psql -d competences_class_9b < /path/to/backend/schema.sql
# ... etc
```

### Database Backups

**Manual backup:**
```bash
sudo -u postgres pg_dump competences_class_9a > backup_9a.sql
```

**Automated backups with NixOS:**
```nix
services.postgresqlBackup = {
  enable = true;
  databases = [
    "competences_class_9a"
    "competences_class_9b"
    "competences_class_9c"
    "competences_class_9d"
  ];
  startAt = "*-*-* 02:00:00";  # Daily at 2 AM
};
```

Backups are stored in `/var/backup/postgresql/`.

## Monitoring and Maintenance

### Service Status

**Check all instances:**
```bash
systemctl list-units 'competences-*' --all
```

**Check specific instance:**
```bash
systemctl status competences-class-9a.service
```

### Logs

**Follow logs for an instance:**
```bash
journalctl -u competences-class-9a.service -f
```

**View recent logs:**
```bash
journalctl -u competences-class-9a.service -n 100
```

**Logs since last boot:**
```bash
journalctl -u competences-class-9a.service -b
```

### Database Health

**Check database size:**
```bash
sudo -u postgres psql -d competences_class_9a -c "
  SELECT pg_size_pretty(pg_database_size('competences_class_9a'));
"
```

**Check command count:**
```bash
sudo -u postgres psql -d competences_class_9a -c "
  SELECT COUNT(*) AS total_commands FROM commands;
"
```

**Check snapshot count:**
```bash
sudo -u postgres psql -d competences_class_9a -c "
  SELECT COUNT(*) AS total_snapshots FROM snapshots;
"
```

**Check latest generation:**
```bash
sudo -u postgres psql -d competences_class_9a -c "
  SELECT MAX(generation) AS latest_generation FROM commands;
"
```

### Nginx Status

**Check nginx configuration:**
```bash
nginx -t
```

**Check virtual hosts:**
```bash
nginx -T | grep server_name
```

**SSL certificate status:**
```bash
systemctl status acme-9a.competences.example.com.service
```

## Troubleshooting

### Service Won't Start

**1. Check logs:**
```bash
journalctl -u competences-class-9a.service -n 50 --no-pager
```

**2. Common issues:**

| Error | Solution |
|-------|----------|
| "Failed to read config file" | Check `secretsFile` path and permissions |
| "No document found in database" | Initialize with `--init-document` or restore from backup |
| "Failed to connect to database" | Ensure PostgreSQL is running and database exists |
| "Address already in use" | Port conflict - check `ss -tlnp \| grep <port>` |
| "Schema version mismatch" | Run schema migration or reinitialize database |

**3. Test manually:**
```bash
# Run backend directly to see errors
sudo -u competences-class-9a \
  /nix/store/.../bin/competences-backend \
    --port 8081 \
    --database "host=/run/postgresql dbname=competences_class_9a" \
    --config /run/agenix/competences-9a \
    --static /var/lib/competences/static
```

### Frontend Not Loading

**1. Check static files:**
```bash
ls -la /var/lib/competences/static/
# Should contain: app.wasm, ghc_wasm_jsffi.js, index.js
```

**2. Check nginx logs:**
```bash
journalctl -u nginx.service -f
```

**3. Check browser console:**
- Open browser developer tools
- Look for 404 errors on WASM/JS files
- Check for CORS or CSP errors

### Database Connection Issues

**1. Verify PostgreSQL is running:**
```bash
systemctl status postgresql.service
```

**2. Verify database exists:**
```bash
sudo -u postgres psql -l | grep competences
```

**3. Check database schema:**
```bash
sudo -u postgres psql -d competences_class_9a -c "\dt"
# Should show: commands, snapshots, metadata, schema_migrations, startup_log
```

**4. Test connection:**
```bash
sudo -u postgres psql -d competences_class_9a -c "SELECT version();"
```

### WebSocket Connection Failures

**1. Check nginx WebSocket configuration:**
```bash
nginx -T | grep -A 10 "proxy_pass.*8081"
# Should include: proxy_http_version 1.1; upgrade headers
```

**2. Check browser console:**
- Look for WebSocket connection errors
- Verify WSS (secure WebSocket) is used on HTTPS sites

**3. Test WebSocket directly:**
```bash
# Install websocat
nix-shell -p websocat

# Test connection (replace with your JWT token)
websocat "wss://9a.competences.example.com/?token=YOUR_JWT_HERE"
```

### Secrets Not Accessible

**1. Check agenix service:**
```bash
systemctl status agenix.service
```

**2. Verify secret was decrypted:**
```bash
ls -la /run/agenix/
# Should contain your secrets
```

**3. Check permissions:**
```bash
ls -la /run/agenix/competences-9a
# Should be readable by the service user
```

**4. Re-decrypt manually:**
```bash
# On your PC
agenix -d secrets/9a-config.json.age
# Verify the content is valid JSON
```

## Security Considerations

### Secrets

- ✅ **DO** use agenix for all secrets
- ✅ **DO** use strong random JWT secrets (≥32 characters)
- ❌ **DON'T** commit plaintext secrets
- ❌ **DON'T** log secrets in application code

### Network

- ✅ **DO** use HTTPS everywhere (enableACME + forceSSL)
- ✅ **DO** keep firewall enabled
- ✅ **DO** use proper OAuth2 scopes
- ❌ **DON'T** expose backend ports directly

### Database

- ✅ **DO** use Unix socket connections (`host=/run/postgresql`)
- ✅ **DO** regular backups
- ✅ **DO** test restore procedures
- ❌ **DON'T** expose PostgreSQL to the internet

### Updates

- ✅ **DO** keep NixOS updated
- ✅ **DO** monitor security advisories
- ✅ **DO** test updates in staging environment
- ❌ **DON'T** auto-update without testing

## Example: Complete Production Setup

See `docs/example-nixos-config.nix` for a fully commented example with:
- 4 class instances
- Agenix secrets integration
- PostgreSQL setup
- Nginx with HTTPS
- Monitoring commands
- Backup configuration

## Further Reading

- [NixOS Manual](https://nixos.org/manual/nixos/stable/)
- [Nix Flakes](https://nixos.wiki/wiki/Flakes)
- [agenix Documentation](https://github.com/ryantm/agenix)
- [Nginx Reverse Proxy Guide](https://docs.nginx.com/nginx/admin-guide/web-server/reverse-proxy/)
- [PostgreSQL Documentation](https://www.postgresql.org/docs/)
