# ============================================================================
# TEMPLATE: Container Configuration for Testing Competences
# ============================================================================
#
# This is a TEMPLATE file. You must customize it before use!
#
# Usage:
#   1. Copy this file: cp docs/test-container.nix my-test-config.nix
#   2. Customize the configuration below (see CUSTOMIZE markers)
#   3. Run: ./test-container.sh my-test-config.nix
#
# What to customize:
#   - secretsFile: Point to your local secrets JSON file
#   - initDocument: Point to your initial data JSON file (optional)
#   - OAuth2 credentials in secretsFile
#
# ============================================================================

{ config, pkgs, lib, ... }:

let
  # CUSTOMIZE: Change to github:marmay/competences for production
  # Or use path:/your/local/path for local development
  competencesFlake = builtins.getFlake "github:marmay/competences";

  # Get the module and packages
  competencesModule = competencesFlake.nixosModules.competences;
  competencesPackages = competencesFlake.packages.${pkgs.system};

  # Generate self-signed certificate for *.local.test
  # This is for TESTING ONLY - use proper certificates in production
  selfSignedCert = pkgs.runCommand "test-local-cert" {
    buildInputs = [ pkgs.openssl ];
  } ''
    mkdir -p $out

    # Generate private key
    openssl genrsa -out $out/key.pem 2048

    # Generate certificate (valid for 10 years)
    # Supports *.local.test and local.test domains
    # Note: Direct IP access will show certificate warning (expected for testing)
    openssl req -new -x509 -key $out/key.pem -out $out/cert.pem -days 3650 \
      -subj "/CN=*.local.test/O=Competences Test/C=US" \
      -addext "subjectAltName=DNS:*.local.test,DNS:local.test"
  '';

in
{
  imports = [ competencesModule ];

  # Make competences packages available
  nixpkgs.overlays = [
    (final: prev: {
      competences-backend = competencesPackages.competences-backend;
      competences-frontend = competencesPackages.competences-frontend;
    })
  ];

  # Basic system configuration
  system.stateVersion = "25.11";

  # Networking
  networking = {
    firewall.allowedTCPPorts = [ 80 443 8080 ];
    # Use host's DNS for internet access (needed for OAuth)
    useHostResolvConf = lib.mkForce true;
  };

  # Competences service configuration
  services.competences = {
    enable = true;

    instances.class-9a = {
      port = 8080;
      subdomain = "class-9a";  # Will be: class-9a.local.test
      database = "competences_test_9a";

      # CUSTOMIZE: Point to your local secrets file
      # Example:
      #   secretsFile = /path/to/your/secrets.json;
      #
      # Expected JSON format:
      # {
      #   "jwtSecret": "your-secret-key-here-minimum-32-characters",
      #   "oauth2": {
      #     "clientId": "your-azure-client-id",
      #     "clientSecret": "your-azure-client-secret",
      #     "redirectUri": "https://class-9a.test.local/oauth/callback",
      #     "tenantId": "your-azure-tenant-id"
      #   }
      # }
      secretsFile = pkgs.writeText "config.json" (builtins.toJSON {
        jwtSecret = "test-jwt-secret-minimum-32-characters-long-for-testing";
        oauth2 = {
          clientId = "REPLACE-WITH-YOUR-AZURE-CLIENT-ID";
          clientSecret = "REPLACE-WITH-YOUR-AZURE-CLIENT-SECRET";
          redirectUri = "https://class-9a.local.test/oauth/callback";
          tenantId = "REPLACE-WITH-YOUR-AZURE-TENANT-ID";
        };
      });

      # CUSTOMIZE: Point to your initial document (optional)
      # Example:
      #   initDocument = /path/to/your/initial-data.json;
      #
      # If not provided, database will start empty
      # Expected JSON format: see common/lib/Competences/Document.hs
      initDocument = pkgs.writeText "test-doc.json" (builtins.toJSON {
        users = [];
        competences = [];
        competenceGrids = [];
        evidences = [];
        resources = [];
        templates = [];
        locks = [];
        checksums = {
          users = "";
          competences = "";
          competenceGrids = "";
          evidences = "";
          resources = "";
          templates = "";
          locks = "";
        };
        partialChecksums = [];
        overallChecksum = "";
      });
    };

    # Nginx reverse proxy with HTTPS (self-signed cert for testing)
    nginx = {
      enable = true;
      domain = "local.test";  # Instances will be: <subdomain>.local.test
      enableACME = false;  # Using self-signed cert instead
      forceSSL = true;     # Redirect HTTP to HTTPS
    };

    # PostgreSQL database
    postgresql.enable = true;
  };

  # Configure Nginx to use self-signed certificate
  services.nginx.virtualHosts."class-9a.local.test" = {
    sslCertificate = "${selfSignedCert}/cert.pem";
    sslCertificateKey = "${selfSignedCert}/key.pem";
  };
}
