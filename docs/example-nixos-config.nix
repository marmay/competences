# Example NixOS configuration for deploying Competences
#
# This shows how to configure the Competences tracking system
# on a NixOS server with 4 class instances.

{ config, pkgs, ... }:

{
  # Import the Competences module
  # In your flake-based config, this is done via:
  #   inputs.competences.nixosModules.competences

  # Enable and configure Competences service
  services.competences = {
    enable = true;

    # Optional: override default packages
    # package = inputs.competences.packages.x86_64-linux.competences-backend;
    # frontendPackage = inputs.competences.packages.x86_64-linux.competences-frontend;

    # Optional: customize static files directory
    # staticDir = "/var/lib/competences/static";

    # Configure instances (one per class)
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

    # Enable Nginx reverse proxy with automatic HTTPS
    nginx = {
      enable = true;
      domain = "competences.example.com";
      enableACME = true;  # Let's Encrypt
      forceSSL = true;
    };

    # Automatically create PostgreSQL databases
    postgresql = {
      enable = true;
      # package = pkgs.postgresql_15;  # Optional: specify version
    };
  };

  # Configure agenix secrets
  # See: https://github.com/ryantm/agenix
  age.secrets = {
    competences-9a.file = ./secrets/9a-config.json.age;
    competences-9b.file = ./secrets/9b-config.json.age;
    competences-9c.file = ./secrets/9c-config.json.age;
    competences-9d.file = ./secrets/9d-config.json.age;
  };

  # Ensure PostgreSQL is configured
  # (only needed if not using services.competences.postgresql.enable)
  # services.postgresql = {
  #   enable = true;
  #   ensureDatabases = [
  #     "competences_class_9a"
  #     "competences_class_9b"
  #     "competences_class_9c"
  #     "competences_class_9d"
  #   ];
  # };

  # Open firewall ports for Nginx (if needed)
  networking.firewall.allowedTCPPorts = [ 80 443 ];

  # Optional: Manual database initialization
  # If you need to initialize the database schema manually:
  #
  # 1. SSH to server
  # 2. sudo -u postgres psql -d competences_class_9a < /path/to/schema.sql
  # 3. Restart service: systemctl restart competences-class-9a.service
}

# --- Flake-based configuration example ---
#
# Your flake.nix on the server might look like:
#
# {
#   inputs = {
#     nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
#     competences.url = "github:youruser/competences";  # or "path:/path/to/competences"
#     agenix.url = "github:ryantm/agenix";
#   };
#
#   outputs = { self, nixpkgs, competences, agenix }: {
#     nixosConfigurations.yourserver = nixpkgs.lib.nixosSystem {
#       system = "x86_64-linux";
#       modules = [
#         agenix.nixosModules.default
#         competences.nixosModules.competences
#         ./configuration.nix
#       ];
#       specialArgs = { inherit competences; };
#     };
#   };
# }

# --- Deployment from your PC ---
#
# Build and deploy to server:
#   sudo nixos-rebuild switch \
#     --flake .#yourserver \
#     --target-host root@server \
#     --build-host localhost
#
# This builds on your powerful PC and deploys to the server.

# --- Secrets management with agenix ---
#
# 1. Create secrets file (config.json):
# {
#   "jwtSecret": "your-secret-key-here-minimum-32-characters",
#   "oauth2": {
#     "clientId": "your-azure-client-id",
#     "clientSecret": "your-azure-client-secret",
#     "redirectUri": "https://9a.competences.example.com/oauth/callback",
#     "tenantId": "your-azure-tenant-id"
#   }
# }
#
# 2. Encrypt with agenix:
#   agenix -e secrets/9a-config.json.age
#
# 3. Commit encrypted file to repo
#
# 4. On server, agenix will decrypt to temporary path which is passed via secretsFile

# --- Monitoring and logs ---
#
# View service status:
#   systemctl status competences-class-9a.service
#
# View logs:
#   journalctl -u competences-class-9a.service -f
#
# List all instances:
#   systemctl list-units 'competences-*'
