{ profiles, config, ... }:
{

  sops.secrets = {
    "ssh/freeman.xiong/id_ed25519" = {
      path = "${config.users.users."freeman.xiong".home}/.ssh/id_ed25519";
      mode = "600";
      owner = "freeman.xiong";
    };

  };
  home-manager = {
    users = {
      "freeman.xiong" = {
        home = {
          file = {
            ".ssh/id_ed25519.pub" = {
              text = profiles.share.users-dict."freeman.xiong".public-key;
              executable = false;
            };
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
              "mail*" = {
                hostname = "43.156.66.157";
                # forwardX11 = true;
                # forwardX11Trusted = true;
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
                hostname = "18.163.206.49";
              };
              "heco-nginx*" = {
                hostname = "54.255.82.12";
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
      };
    };
  };
}
