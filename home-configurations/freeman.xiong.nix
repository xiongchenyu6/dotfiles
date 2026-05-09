{
  pkgs,
  shares,
  config,
  ezModules,
  osConfig,
  lib,
  inputs,
  ...
}:
let
  # Check if we're on Darwin by checking if osConfig has the darwin-specific attributes
  # If osConfig doesn't have system.nixos, we're likely on Darwin
  hasNixOSTags = osConfig ? system && osConfig.system ? nixos && osConfig.system.nixos ? tags;
  hasGuiTag = hasNixOSTags && (builtins.elem "gui" osConfig.system.nixos.tags);
  hasNvidiaTag = hasNixOSTags && (builtins.elem "nvidia" osConfig.system.nixos.tags);
  isDarwin = !hasNixOSTags;
  hostName = if hasNixOSTags then osConfig.networking.hostName else "";
  isVps =
    hasNixOSTags
    && builtins.elem hostName [
      "oracle-arm-001"
      "oracle-arm-002"
      "oracle-amd-001"
      "oracle-amd-002"
      "tcloud"
      "netbird"
      "huoshan-bj-001"
    ];
  enableHomeSops = (isDarwin || hasGuiTag) && !isVps;
in
{
  imports =
    # Import mac-app-util only on macOS for proper application linking
    (lib.optionals isDarwin [
    ])
    ++
      # Platform-specific module imports
      (
        if isDarwin then
          [
            ezModules.zsh
            ezModules.cli-development
            ezModules.gui
          ]
        else
          (
            if hasGuiTag then
              [
                ezModules.zsh
                ezModules.cli-development
                ezModules.gui
                #ezModules.mpd
                ezModules.stow-config
                ezModules.niri
                ezModules.tmux
              ]
            else
              [
                ezModules.zsh
                ezModules.cli-server
                ezModules.tmux
              ]
          )
          ++ (
            if hasNvidiaTag then
              [
                ezModules.nvidia
              ]
            else
              [ ]
          )
      );

  # Only enable SOPS secrets on desktop (Darwin/GUI) - not on VPS servers
  sops.secrets = lib.mkIf enableHomeSops {
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
  sops.templates."api-keys-env" = lib.mkIf enableHomeSops {
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
      EDITOR = lib.mkForce "nvim";
    };
    homeDirectory = osConfig.users.users."freeman.xiong".home;
    sessionPath = [ "$HOME/.local/bin" ];
  };

  # Sunshine on Wayland+NVIDIA: zwlr_screencopy returns unusable dmabufs from
  # the NVIDIA driver, so Moonlight clients see corrupted/garbled frames. KMS
  # capture (Sunshine reads the GPU framebuffer directly via DRM, requires
  # capSysAdmin which the system module already grants) bypasses Wayland for
  # the capture path. encoder=nvenc tries the GPU encoder first; falls back
  # to libx264 when libcuda.so.1 isn't reachable from the unit's lib path.
  xdg.configFile."sunshine/sunshine.conf" = lib.mkIf hasGuiTag {
    text = ''
      address_family = ipv4
      upnp = disabled
      capture = kms
      encoder = nvenc
      origin_web_ui_allowed = wan
    '';
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
      settings = {
        push = {
          default = "current";
        };
        color = {
          ui = "auto";
        };
        core = {
          autocrlf = "input";
          editor = "nvim";
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
      # forwardAgent moved to matchBlocks."*" as per new format
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
        "sg-office" = {
          hostname = "101.78.126.6";
        };
        "sz-office" = {
          hostname = "113.90.155.31";
          user = "autolife";
          port = 2222;
        };
        "gz-office" = {
          hostname = "183.6.107.47";
          user = "autolife";
          port = 2222;
        };
        "lubancat" = {
          hostname = "203.116.95.146";
          user = "root";
        };
      };
    };
  };

  # Systemd user service to ensure environment directory exists
  # The actual environment file is created by sops template above
  systemd.user.services.setup-environment = lib.mkIf enableHomeSops {
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

  # Load API keys from sops into shell environment
  programs.zsh = {
    initContent = lib.optionalString enableHomeSops ''
      # Source API keys from sops if the file exists
      if [ -f "${config.home.homeDirectory}/.config/sops-nix/secrets/rendered/api-keys-env" ]; then
        set -a  # mark all new variables for export
        source "${config.home.homeDirectory}/.config/sops-nix/secrets/rendered/api-keys-env"
        set +a
      fi
    '';
  };

}
