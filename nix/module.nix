# NixOS module for Competences tracking system
{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.competences;

  # Instance type definition
  instanceOpts = { name, ... }: {
    options = {
      port = mkOption {
        type = types.port;
        description = "Port for this instance to listen on";
      };

      subdomain = mkOption {
        type = types.str;
        description = "Subdomain for this instance (e.g., '9a' for 9a.competences.example.com)";
      };

      database = mkOption {
        type = types.str;
        description = "PostgreSQL database name for this instance";
      };

      secretsFile = mkOption {
        type = types.path;
        description = ''
          Path to configuration file containing secrets (JWT secret and OAuth2 config).
          This should typically be an agenix-managed secret.

          Expected JSON format:
          {
            "jwtSecret": "your-secret-key-here-minimum-32-characters",
            "oauth2": {
              "clientId": "your-azure-client-id",
              "clientSecret": "your-azure-client-secret",
              "redirectUri": "https://9a.competences.example.com/oauth/callback",
              "tenantId": "your-azure-tenant-id"
            }
          }
        '';
      };

      initDocument = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = ''
          Path to initial document JSON file.
          Only used if database is empty on first startup.
        '';
      };

      user = mkOption {
        type = types.str;
        default = "competences-${name}";
        description = "User account under which this instance runs";
      };

      group = mkOption {
        type = types.str;
        default = "competences";
        description = "Group under which this instance runs";
      };
    };
  };

in {
  options.services.competences = {
    enable = mkEnableOption "Competences tracking system";

    package = mkOption {
      type = types.package;
      default = pkgs.competences-backend or (throw "competences-backend package not found. Please ensure the flake is properly configured.");
      description = "The competences-backend package to use";
    };

    frontendPackage = mkOption {
      type = types.package;
      default = pkgs.competences-frontend or (throw "competences-frontend package not found. Please ensure the flake is properly configured.");
      description = "The competences-frontend package to use";
    };

    staticDir = mkOption {
      type = types.path;
      default = "/var/lib/competences/static";
      description = "Directory where frontend static files will be deployed";
    };

    instances = mkOption {
      type = types.attrsOf (types.submodule instanceOpts);
      default = {};
      description = "Competences instances to run (one per class)";
      example = literalExpression ''
        {
          class-9a = {
            port = 8081;
            subdomain = "9a";
            database = "competences_class_9a";
            secretsFile = config.age.secrets.competences-9a.path;
            initDocument = ./data/class_9a.json;
          };
        }
      '';
    };

    nginx = {
      enable = mkEnableOption "Nginx reverse proxy configuration";

      domain = mkOption {
        type = types.str;
        description = "Base domain for the competences instances (e.g., 'competences.example.com')";
        example = "competences.example.com";
      };

      enableACME = mkOption {
        type = types.bool;
        default = true;
        description = "Enable ACME/Let's Encrypt for HTTPS";
      };

      forceSSL = mkOption {
        type = types.bool;
        default = true;
        description = "Force SSL/HTTPS redirects";
      };
    };

    postgresql = {
      enable = mkEnableOption "Automatic PostgreSQL database setup";

      package = mkOption {
        type = types.package;
        default = pkgs.postgresql;
        description = "PostgreSQL package to use";
      };
    };
  };

  config = mkIf cfg.enable {
    # Ensure PostgreSQL is enabled if automatic setup is requested
    services.postgresql = mkIf cfg.postgresql.enable {
      enable = true;
      package = cfg.postgresql.package;
      ensureDatabases = map (name: cfg.instances.${name}.database) (attrNames cfg.instances);
      # Note: Users still need to be created manually or via ensureUsers
    };

    # Deploy frontend static files
    system.activationScripts.competences-frontend = ''
      mkdir -p ${cfg.staticDir}
      cp -r ${cfg.frontendPackage}/* ${cfg.staticDir}/
      chmod -R 755 ${cfg.staticDir}
    '';

    # Create system users and groups
    users.groups.competences = {};

    users.users = listToAttrs (map (name:
      let instance = cfg.instances.${name}; in
      nameValuePair instance.user {
        isSystemUser = true;
        group = instance.group;
        description = "Competences instance ${name} user";
      }
    ) (attrNames cfg.instances));

    # Generate systemd services for each instance
    systemd.services = listToAttrs (map (name:
      let
        instance = cfg.instances.${name};
        serviceName = "competences-${name}";
      in
      nameValuePair serviceName {
        description = "Competences tracking system instance: ${name}";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ] ++ optional cfg.postgresql.enable "postgresql.service";
        requires = optional cfg.postgresql.enable "postgresql.service";

        serviceConfig = {
          Type = "simple";
          User = instance.user;
          Group = instance.group;
          Restart = "always";
          RestartSec = "10s";

          ExecStart = concatStringsSep " " ([
            "${cfg.package}/bin/competences-backend"
            "--port ${toString instance.port}"
            "--database \"host=/run/postgresql dbname=${instance.database}\""
            "--config ${instance.secretsFile}"
            "--static ${cfg.staticDir}"
          ] ++ optional (instance.initDocument != null) "--init-document ${instance.initDocument}");

          # Security hardening
          NoNewPrivileges = true;
          PrivateTmp = true;
          ProtectSystem = "strict";
          ProtectHome = true;
          ReadWritePaths = [ ];
        };
      }
    ) (attrNames cfg.instances));

    # Configure Nginx reverse proxy
    services.nginx = mkIf cfg.nginx.enable {
      enable = true;

      recommendedProxySettings = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;

      virtualHosts = listToAttrs (map (name:
        let
          instance = cfg.instances.${name};
          fqdn = "${instance.subdomain}.${cfg.nginx.domain}";
        in
        nameValuePair fqdn {
          enableACME = cfg.nginx.enableACME;
          forceSSL = cfg.nginx.forceSSL;

          locations."/" = {
            proxyPass = "http://127.0.0.1:${toString instance.port}";
            proxyWebsockets = true;
            extraConfig = ''
              proxy_set_header Host $host;
              proxy_set_header X-Real-IP $remote_addr;
              proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
              proxy_set_header X-Forwarded-Proto $scheme;
            '';
          };
        }
      ) (attrNames cfg.instances));
    };
  };
}
