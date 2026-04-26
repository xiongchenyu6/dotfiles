# Server CLI tier — extends cli-minimal with dev/ops tools
{ config, pkgs, lib, ... }:
{
  home.packages = [
    pkgs.mosh
  ];

  editorconfig = {
    enable = true;
    settings = {
      "*" = {
        indent_size = 2;
        end_of_line = "lf";
        insert_final_newline = true;
      };
      "*.{js,py}" = {
        charset = "utf-8";
      };
      "*.css" = {
        charset = "utf-8";
      };
      "*.{py,cpp,c,h,proto}" = {
        indent_style = "space";
        indent_size = 4;
      };

      "Makefile" = {
        indent_style = "tab";
      };
      "lib/**.js" = {
        indent_style = "space";
      };
      "{package.json,.travis.yml}" = {
        indent_style = "space";
      };
    };
  };

  services = {
    gpg-agent = {
      enable = true;
      enableExtraSocket = true;
      pinentry.package = lib.mkDefault pkgs.pinentry-curses;
      enableSshSupport = true;
      sshKeys = [
        "AB721FF9682FF07B88063C8FADEB89B859C7ACB1"
      ];
    };
  };

  programs = {
    readline = {
      enable = true;
    };

    bat = {
      enable = true;
    };

    nix-index = {
      enable = true;
    };

    topgrade = {
      enable = true;
      settings = {
        misc = {
          pre_sudo = false;
          only = [
            "system"
            "tldr"
          ];
        };
        linux = { };
      };
    };

    gh = {
      enable = true;
      settings = {
        git_protocal = "ssh";
      };
    };

    script-directory = {
      enable = true;
      settings = {
        SD_ROOT = "${config.home.homeDirectory}/dotfiles/scripts";
        SD_EDITOR = "nvim";
        SD_CAT = "bat";
      };
    };
  };
}
