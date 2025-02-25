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
  inherit (pkgs.stdenv) isDarwin;
in
{

  imports =
    lib.debug.traceSeq osConfig.system.nixos.tags (
      if (builtins.elem "gui" osConfig.system.nixos.tags) then
        [
          ezModules.zsh
          ezModules.cli
          ezModules.gui
          ezModules.mpd
          ezModules.stow-config
          ezModules.dvorak
          ezModules.nvidia
          ezModules.hyprland
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

  sops.secrets = lib.mkIf (builtins.elem "gui" osConfig.system.nixos.tags) {
    "ssh/freeman.xiong/id_ed25519" = {
      path = "${osConfig.users.users."freeman.xiong".home}/.ssh/id_ed25519";
      mode = "600";
    };
  };

  home = {
    file = {
      ".ssh/id_ed25519.pub" = {
        text = shares.users-dict."freeman.xiong".public-key;
        executable = false;
      };
    };
    sessionVariables = {
      OPENAI_API_BASE = "https://api.siliconflow.cn/v1";
      OPENAI_API_KEY = builtins.getEnv "SILICON_FLOW";
    };
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
      matchBlocks = {
        "*-tmux" = {
          extraOptions = {
            RequestTTY = "yes";
            RemoteCommand = "tmux new -A -s xiongchenyu";
          };
        };
        "tcloud*" = {
          hostname = "43.156.66.157";
          # forwardX11 = true;
          # forwardX11Trusted = true;
        };
        "oracle-arm*" = {
          hostname = "138.2.95.174";
        };
        "oracle-amd-001*" = {
          hostname = "213.35.97.233";
        };
        "oracle-amd-002*" = {
          hostname = "213.35.117.232";
        };
        "office*" = {
          hostname = "172.22.240.98";
        };
        "game*" = {
          hostname = "172.22.240.99";
        };
        "digital*" = {
          hostname = "143.198.87.228";
        };
        "netbird*" = {
          hostname = "47.128.253.85";
        };
        "heco-nginx*" = {
          hostname = "13.229.128.55";
          user = "root";
        };
        "heco-zammad*" = {
          hostname = "10.16.0.96";
          user = "root";
          proxyJump = "heco-nginx";
        };
        "heco-mysql*" = {
          hostname = "10.16.0.230";
          user = "root";
          proxyJump = "heco-nginx";
        };
      };
    };
  };
}
