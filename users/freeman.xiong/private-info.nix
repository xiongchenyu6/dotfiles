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
                condition = "gitdir:**/Private/**/.git";
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
                condition = "gitdir:**/Workspace/**/.git";
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
              fetch = {
                writeCommitMessage = true;
              };
              push = {
                default = "current";
              };
              color = {
                ui = "auto";
              };
              core = {
                autocrlf = "input";
                editor = "emacs";
                untrackedCache = true;
                fsmonitor = true;
              };
              pull = {
                rebase = false;
              };
              user = {
                name = "xiongchenyu";
                email = "xiongchenyu6@gmail.com";
                useConfigOnly = true;
              };
              rerere = {
                enabled = true;
              };
              column = {
                ui = "auto";
              };
            };
          };

          ssh = {
            matchBlocks = {
              "mail" = {
                port = 2222;
                hostname = "43.156.66.157";
                forwardX11 = true;
                forwardX11Trusted = true;
              };
              "digital" = {
                port = 2222;
                hostname = "206.189.156.130";
                user = "root";
                forwardX11 = true;
                forwardX11Trusted = true;
              };
              "netbird" = {
                port = 2222;
                hostname = "167.172.91.53";
                user = "root";
                forwardX11 = true;
                forwardX11Trusted = true;
              };
              "heco-nginx" = {
                hostname = "13.215.48.98";
                user = "root";
                forwardX11 = true;
                forwardX11Trusted = true;
              };
              "heco-k3s-office-main" = {
                hostname = "10.16.1.164";
                user = "root";
                forwardX11 = true;
                forwardX11Trusted = true;
                proxyJump = "heco-nginx";
              };
              "heco-netbird" = {
                hostname = "13.215.48.98";
                user = "root";
                forwardX11 = true;
                forwardX11Trusted = true;
                proxyJump = "heco-nginx";
              };

            };
          };
        };
      };
    };
  };
}
