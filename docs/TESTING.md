# Testing Deployment Locally

This guide covers testing the Competences deployment in a local NixOS container before deploying to production.

## Quick Start

```bash
# Run the test script
./test-container.sh
```

The script will:
1. Create a NixOS container named `competences-test`
2. Configure it with the Competences service
3. Show you the container IP and next steps

## Step-by-Step Manual Setup

If you prefer to do it manually:

### 1. Create and Start Container

```bash
sudo nixos-container create competences-test --config-file docs/test-container.nix
sudo nixos-container start competences-test
```

### 2. Get Container IP

```bash
IP=$(sudo nixos-container show-ip competences-test)
echo "Container IP: $IP"
```

### 3. Add to /etc/hosts

```bash
echo "$IP class-9a.test.local" | sudo tee -a /etc/hosts
```

### 4. Initialize Database

The NixOS module automatically creates the PostgreSQL database, but you need to initialize it:

```bash
# Enter container
sudo nixos-container root-login competences-test

# Inside container: Check if database exists
sudo -u postgres psql -lqt | grep competences_test_9a

# If not exists, create it
sudo -u postgres createdb competences_test_9a

# Exit container
exit
```

### 5. Start the Service

```bash
# Start the service
sudo nixos-container run competences-test -- systemctl start competences-9a

# Check status
sudo journalctl -M competences-test -u competences-9a -f
```

### 6. Access the Application

Visit: http://class-9a.test.local

## Useful Commands

### Container Management

```bash
# Start container
sudo nixos-container start competences-test

# Stop container
sudo nixos-container stop competences-test

# Restart container
sudo nixos-container restart competences-test

# Destroy container (delete all data)
sudo nixos-container destroy competences-test

# List all containers
sudo nixos-container list
```

### Accessing the Container

```bash
# Root shell
sudo nixos-container root-login competences-test

# Run command in container
sudo nixos-container run competences-test -- <command>
```

### Viewing Logs

```bash
# Follow service logs
sudo journalctl -M competences-test -u competences-9a -f

# View PostgreSQL logs
sudo journalctl -M competences-test -u postgresql -f

# View Nginx logs
sudo journalctl -M competences-test -u nginx -f

# All logs
sudo journalctl -M competences-test -f
```

### Debugging

```bash
# Check service status
sudo nixos-container run competences-test -- systemctl status competences-9a

# Check database status
sudo nixos-container run competences-test -- systemctl status postgresql

# Check nginx status
sudo nixos-container run competences-test -- systemctl status nginx

# Test database connection
sudo nixos-container root-login competences-test
sudo -u postgres psql -d competences_test_9a -c "SELECT version();"
```

### Updating the Container

If you make changes to the configuration:

```bash
# Rebuild the container
sudo nixos-container update competences-test --config-file docs/test-container.nix

# Restart services
sudo nixos-container run competences-test -- systemctl restart competences-9a
```

## Testing Different Configurations

You can create multiple test containers for different scenarios:

```bash
# Test with 4 instances (like production)
sudo nixos-container create competences-multi --config-file docs/test-multi-instance.nix

# Test with custom secrets
sudo nixos-container create competences-secrets --config-file docs/test-with-secrets.nix
```

## Cleanup

When you're done testing:

```bash
# Stop and destroy container
sudo nixos-container stop competences-test
sudo nixos-container destroy competences-test

# Remove from /etc/hosts
sudo sed -i '/class-9a.test.local/d' /etc/hosts
```

## Differences from Production

The test container uses:
- Hardcoded test secrets (INSECURE)
- No HTTPS/ACME
- Local .test.local domain
- Ephemeral storage (lost on container destroy)
- Simple networking (no external access)

For production deployment, see [DEPLOYMENT.md](DEPLOYMENT.md).
