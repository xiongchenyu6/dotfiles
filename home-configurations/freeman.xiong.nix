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
  isDarwin =
    (builtins ? "currentSystem")
    && (builtins.currentSystem == "x86_64-darwin" || builtins.currentSystem == "aarch64-darwin");
  #isDarwin = false;
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
        if (builtins.elem "gui" osConfig.system.nixos.tags) then
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
        if (builtins.elem "nvidia" osConfig.system.nixos.tags) then
          [
            ezModules.nvidia
          ]
        else
          [ ]
      );

  sops.secrets = lib.mkIf (isDarwin || (builtins.elem "gui" osConfig.system.nixos.tags)) {
    "ssh/freeman.xiong/id_ed25519" = {
      path = "${osConfig.users.users."freeman.xiong".home}/.ssh/id_ed25519";
      mode = "600";
    };
  };

  home = {
    username = "freeman.xiong";
    file = {
      ".ssh/id_ed25519.pub" = {
        text = shares.users-dict."freeman.xiong".public-key;
        executable = false;
      };
    };
    sessionVariables = {
      OPENAI_API_BASE = "https://api.siliconflow.cn/v1";
      OPENAI_API_KEY = builtins.getEnv "SILICON_FLOW";
      XAI_API_KEY = builtins.getEnv "XAI_API_KEY";
      OPENROUTER_API_KEY = builtins.getEnv "OPENROUTER_API_KEY";
      GEMINI_API_KEY = builtins.getEnv "GEMINI_API_KEY";
      GITHUB_PERSONAL_ACCESS_TOKEN = builtins.getEnv "Github_Access_Token";
      SLACK_BOT_TOKEN = builtins.getEnv "SLACK_BOT_TOKEN";
      SLACK_TEAM_ID = builtins.getEnv "SLACK_TEAM_ID";
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
}
