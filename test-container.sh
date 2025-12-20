#!/usr/bin/env bash
# Test Competences deployment in a NixOS container
set -euo pipefail

CONTAINER_NAME="competences"

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

info() {
    echo -e "${BLUE}==>${NC} $1"
}

success() {
    echo -e "${GREEN}==>${NC} $1"
}

error() {
    echo -e "${RED}ERROR:${NC} $1"
    exit 1
}

warning() {
    echo -e "${YELLOW}WARNING:${NC} $1"
}

# Show usage if no config file provided
if [ $# -eq 0 ]; then
    cat <<EOF
${RED}ERROR:${NC} No configuration file provided.

${YELLOW}Usage:${NC}
  $0 <config-file.nix>

${YELLOW}Getting Started:${NC}
  1. Copy the template:
     ${BLUE}cp docs/test-container.nix my-test-config.nix${NC}

  2. Customize the configuration:
     - Edit ${BLUE}my-test-config.nix${NC}
     - Set your OAuth2 credentials in ${BLUE}secretsFile${NC}
     - Optionally provide ${BLUE}initDocument${NC} with initial data
     - See comments in the file for guidance

  3. Run the test container:
     ${BLUE}$0 my-test-config.nix${NC}

${YELLOW}Important:${NC}
  - The template uses GitHub flake: ${BLUE}github:marmay/competences${NC}
  - For local development, change to: ${BLUE}path:/your/local/path${NC}
  - The container generates a self-signed certificate for HTTPS testing
  - Access via: ${BLUE}https://<instance-name>.local.test${NC} (add to /etc/hosts)

${YELLOW}Example:${NC}
  docs/test-container.nix is a template - DO NOT use directly!
  Always copy and customize it first.

EOF
    exit 1
fi

CONFIG_FILE="$1"

# Validate config file exists
if [ ! -f "$CONFIG_FILE" ]; then
    error "Configuration file not found: $CONFIG_FILE"
fi

# Check if running as root
if [ "$EUID" -eq 0 ]; then
    error "Don't run this script as root. It will use sudo when needed."
fi

info "Using configuration: $CONFIG_FILE"
echo

# Check if container already exists
if sudo nixos-container list | grep -q "^$CONTAINER_NAME\$"; then
    warning "Container '$CONTAINER_NAME' already exists."
    read -p "Destroy and recreate? [y/N] " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        info "Stopping container..."
        sudo nixos-container stop "$CONTAINER_NAME" 2>/dev/null || true
        info "Destroying container..."
        sudo nixos-container destroy "$CONTAINER_NAME"
    else
        info "Using existing container."
        sudo nixos-container start "$CONTAINER_NAME" 2>/dev/null || true
        CONTAINER_IP=$(sudo nixos-container show-ip "$CONTAINER_NAME")
        success "Container running at: $CONTAINER_IP"
        echo
        info "Add hostname resolution:"
        echo -e "  ${BLUE}NixOS (permanent):${NC} Add to configuration.nix: networking.extraHosts = ''$CONTAINER_IP <instance-name>.local.test'';"
        echo -e "  ${BLUE}Other distros:${NC} echo \"$CONTAINER_IP <instance-name>.local.test\" | sudo tee -a /etc/hosts"
        echo -e "  ${BLUE}Temporary (any):${NC} Just use IP: https://$CONTAINER_IP"
        echo
        success "Visit: https://<instance-name>.local.test"
        warning "You may need to accept the self-signed certificate in your browser"
        exit 0
    fi
fi

# Create container
info "Creating container '$CONTAINER_NAME'..."
sudo nixos-container create "$CONTAINER_NAME" --config-file "$CONFIG_FILE"

# Reload systemd to register the container unit
info "Reloading systemd..."
sudo systemctl daemon-reload

# Start container
info "Starting container..."
sudo nixos-container start "$CONTAINER_NAME"

# Wait for container to be ready
info "Waiting for container to start..."
sleep 3

# Get container IP
CONTAINER_IP=$(sudo nixos-container show-ip "$CONTAINER_NAME")
success "Container created and started!"
echo
success "Container IP: $CONTAINER_IP"
success "Container name: $CONTAINER_NAME"
echo
info "Next steps:"
echo -e "  1. Add hostname resolution:"
echo -e "     ${BLUE}NixOS (permanent):${NC} Add to configuration.nix:"
echo -e "       ${BLUE}networking.extraHosts = ''${NC}"
echo -e "       ${BLUE}  $CONTAINER_IP <instance-name>.local.test${NC}"
echo -e "       ${BLUE}'';${NC}"
echo -e "       Then: ${BLUE}sudo nixos-rebuild switch${NC}"
echo -e "     ${BLUE}Other distros:${NC}"
echo -e "       ${BLUE}echo \"$CONTAINER_IP <instance-name>.local.test\" | sudo tee -a /etc/hosts${NC}"
echo -e "     ${BLUE}Temporary (any):${NC} Just use IP: ${BLUE}https://$CONTAINER_IP${NC}"
echo
echo -e "  2. Visit: ${BLUE}https://<instance-name>.local.test${NC}"
echo -e "     ${YELLOW}Note: You'll need to accept the self-signed certificate${NC}"
echo
echo -e "  3. Check backend logs:"
echo -e "     ${BLUE}sudo journalctl -M $CONTAINER_NAME -u competences-<instance-name> -f${NC}"
echo
info "Useful commands:"
echo -e "  - Container shell: ${BLUE}sudo nixos-container root-login $CONTAINER_NAME${NC}"
echo -e "  - View logs: ${BLUE}sudo journalctl -M $CONTAINER_NAME -u competences-<instance-name> -f${NC}"
echo -e "  - Restart service: ${BLUE}sudo nixos-container run $CONTAINER_NAME -- systemctl restart competences-<instance-name>${NC}"
echo -e "  - Stop container: ${BLUE}sudo nixos-container stop $CONTAINER_NAME${NC}"
echo -e "  - Destroy container: ${BLUE}sudo nixos-container destroy $CONTAINER_NAME${NC}"
echo
