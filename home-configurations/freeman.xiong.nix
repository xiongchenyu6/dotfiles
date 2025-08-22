{
  pkgs,
  shares,
  config,
  ezModules,
  osConfig,
  lib,
  ...
}:
let
  # Check if we're on Darwin by checking if osConfig has the darwin-specific attributes
  # If osConfig doesn't have system.nixos, we're likely on Darwin
  hasNixOSTags = osConfig ? system && osConfig.system ? nixos && osConfig.system.nixos ? tags;
  hasGuiTag = hasNixOSTags && (builtins.elem "gui" osConfig.system.nixos.tags);
  hasNvidiaTag = hasNixOSTags && (builtins.elem "nvidia" osConfig.system.nixos.tags);
  isDarwin = !hasNixOSTags;
in
{
  imports =
    if isDarwin then
      [
        ezModules.zsh
        ezModules.cli
        ezModules.gui
      ]
    else
      (
        if hasGuiTag then
          [
            ezModules.zsh
            ezModules.cli
            ezModules.gui
            #ezModules.mpd
            ezModules.stow-config
            ezModules.qwert
            ezModules.nvidia
            ezModules.hyprland
            ezModules.tmux
          ]
        else
          [ ezModules.tmux ]
      )
      ++ (
        if hasNvidiaTag then
          [
            ezModules.nvidia
          ]
        else
          [ ]
      );

  # nixpkgs.config removed - using global nixpkgs config from NixOS instead
  # (when home-manager.useGlobalPkgs is enabled)

  sops.secrets = lib.mkIf (isDarwin || hasGuiTag) {
    "ssh/freeman.xiong/id_ed25519" = {
      path = "${osConfig.users.users."freeman.xiong".home}/.ssh/id_ed25519";
      mode = "600";
    };
    # Define API key secrets (they'll be used by the template)
    "api-keys/SILICON_FLOW" = { };
    "api-keys/OPENROUTER_API_KEY" = { };
    "api-keys/GEMINI_API_KEY" = { };
    "api-keys/Github_Access_Token" = { };
    "api-keys/SLACK_BOT_TOKEN" = { };
    "api-keys/SLACK_TEAM_ID" = { };
  };

  # Use sops template for environment variables - much cleaner!
  sops.templates."api-keys-env" = lib.mkIf (isDarwin || hasGuiTag) {
    content = ''
      OPENAI_API_KEY=${config.sops.placeholder."api-keys/SILICON_FLOW"}
      OPENROUTER_API_KEY=${config.sops.placeholder."api-keys/OPENROUTER_API_KEY"}
      GEMINI_API_KEY=${config.sops.placeholder."api-keys/GEMINI_API_KEY"}
      GITHUB_PERSONAL_ACCESS_TOKEN=${config.sops.placeholder."api-keys/Github_Access_Token"}
      SLACK_BOT_TOKEN=${config.sops.placeholder."api-keys/SLACK_BOT_TOKEN"}
      SLACK_TEAM_ID=${config.sops.placeholder."api-keys/SLACK_TEAM_ID"}
    '';
    path = "${config.home.homeDirectory}/.config/environment.d/50-api-keys.conf";
    mode = "600";
  };

  home = {
    username = "freeman.xiong";
    file = {
      ".ssh/id_ed25519.pub" = {
        text = shares.users-dict."freeman.xiong".public-key;
        executable = false;
      };
    };
    # Session variables are now managed by systemd user service
    sessionVariables = {
      OPENAI_API_BASE = "https://api.siliconflow.cn/v1";
    };
    homeDirectory = osConfig.users.users."freeman.xiong".home;
    sessionPath = [ "$HOME/.local/bin" ];
  };
  programs = {
    git = {
      includes = [
        {
          condition = "gitdir:**/github/**/.git";
          contents = {
            user = {
              email = "xiongchenyu6@gmail.com";
              name = "xiongchenyu";
              signingKey = "B99B8189C7C153F6";
            };
            commit = {
              gpgSign = true;
            };
          };
        }
        {
          condition = "gitdir:**/gitlab/tron/**/.git";
          contents = {
            user = {
              email = "freeman.xiong@tron.network";
              name = "freeman.xiong";
              signingKey = "03DFD2DEA7AF6693";
            };
            commit = {
              gpgSign = true;
            };
          };
        }
      ];
      signing = {
        key = "B99B8189C7C153F6";
        signByDefault = true;
      };
      extraConfig = {
        push = {
          default = "current";
        };
        color = {
          ui = "auto";
        };
        core = {
          autocrlf = "input";
          editor = "emacs";
        };
        pull = {
          rebase = false;
        };
        user = {
          name = "xiongchenyu";
          email = "xiongchenyu6@gmail.com";
          useConfigOnly = true;
        };
      };
    };

    ssh = {
      forwardAgent = true;
      matchBlocks = {
        # "*-tmux" = {
        #   extraOptions = {
        #     RequestTTY = "yes";
        #     RemoteCommand = "tmux new -A -s xiongchenyu";
        #   };
        # };
        "tcloud" = {
          hostname = "43.156.66.157";
          # extraOptions = {
          #   RemoteForward = "/run/user/1000/gnupg/S.gpg-agent /run/user/1000/gnupg/S.gpg-agent.extra";
          # };
        };
        "oracle-arm-001" = {
          hostname = "138.2.95.174";
        };
        "oracle-arm-002" = {
          hostname = "138.2.76.211";
        };

        "oracle-amd-001" = {
          hostname = "213.35.97.233";
        };
        "oracle-amd-002" = {
          hostname = "213.35.117.232";
        };
        "office" = {
          hostname = "172.22.240.98";
        };
        "game" = {
          hostname = "172.22.240.99";
        };
        "netbird" = {
          hostname = "18.142.246.191";
        };
        "heco-nginx" = {
          hostname = "18.142.238.159";
          user = "root";
        };
        "heco-zammad" = {
          hostname = "10.16.0.96";
          user = "root";
          proxyJump = "heco-nginx";
        };
        "heco-mysql" = {
          hostname = "10.16.0.230";
          user = "root";
          proxyJump = "heco-nginx";
        };
      };
    };
  };

  # Systemd user service to ensure environment directory exists
  # The actual environment file is created by sops template above
  systemd.user.services.setup-environment = lib.mkIf (isDarwin || hasGuiTag) {
    Unit = {
      Description = "Setup environment directory for API keys";
      After = [ "sops-nix.service" ];
    };
    Service = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.coreutils}/bin/mkdir -p %h/.config/environment.d";
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };
}
