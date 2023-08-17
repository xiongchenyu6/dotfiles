{ profiles, config, ... }: {

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
            signing = {
              key = "5AF7AFBF695E8A5D";
              signByDefault = true;
            };
            extraConfig = {
              push = { default = "current"; };
              color = { ui = "auto"; };
              core = {
                autocrlf = "input";
                editor = "emacs";
              };
              pull = { rebase = false; };
              user = {
                name = "freeman";
                email = "xiongchenyu6@gmail.com";
                useConfigOnly = true;
              };
            };
          };
          ssh = {
            matchBlocks = {
              "mail" = {
                port = 2222;
                hostname = "43.156.66.157";
                user = "root";
                forwardX11 = true;
                forwardX11Trusted = true;
              };
              "digital" = {
                port = 2222;
                user = "root";
              };
              "git-code-commit.*.amazonaws.com" = {
                user = "APKA6ECL465SWNEVSVPZ";
              };
              "*.trontech.link" = { user = "freeman.xiong"; };
            };
          };
        };
      };
    };
  };
}
