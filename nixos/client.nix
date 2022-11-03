{ config, pkgs, lib, symlinkJoin, ... }: {
  imports = let
    ls = dir:
      builtins.map (f: (dir + "/${f}"))
      (builtins.attrNames (builtins.readDir dir));
  in [ ] ++ (ls ./common-apps) ++ (ls ./client-apps) ++ (ls ./common-components)
  ++ (ls ./client-components);

  nix = {
    generateNixPathFromInputs = true;
    generateRegistryFromInputs = true;
    linkInputs = true;
  };

  environment = {
    systemPackages = with pkgs; [
      asciinema
      awscli2
      azure-cli
      agenix
      apg
      clang
      clang-tools
      cmake
      conky
      cabal2nix
      cachix
      discord
      deploy-rs
      neofetch
      exa
      feh
      fd
      jp2a
      jdt-language-server
      pass
      procs
      tealdeer
      tendermint
      socat
      rustscan
      virtualbox
      gitAndTools.gitflow
      git-crypt
      geoip
      gnumake
      gh
      gopls
      graphviz
      gradle2nix
      gcc
      haskell-language-server
      (haskellPackages.ghcWithPackages (self:
        with haskellPackages;
        with pkgs.haskell.lib; [
          apply-refact
          cabal-install
          hlint
          stylish-haskell
          hasktags
          hoogle
          pandoc
        ]))
      heroku
      imagemagick
      ispell
      lsof
      (python3.withPackages (ps:
        with python3.pkgs; [
          my_cookies
          epc
          orjson
          python-lsp-server
          cmake-language-server
        ]))
      pinentry
      patchelf
      linuxPackages.ply
      wakatime
      metals
      nixfmt
      nixpkgs-fmt
      node2nix
      nodejs
      nodePackages."typescript-language-server"
      nodePackages."bash-language-server"
      nvfetcher
      # nix-alien
      # nix-index-update
      nixopsUnstable
      openssl
      protobuf
      plantuml
      ripgrep
      rnix-lsp
      terraform
      terranix
      tronbox
      mysql
      unzip
      vlc
      scrot
      stow
      slack
      statix
      scalafmt
      solc-select
      litecli
      wineWowPackages.staging
      wireshark
      winklink
      #wpa_supplicant_gui
      wakatime
      whatsapp-for-linux
      xclip
      qq
      wechat-uos
      xscreensaver
      ssh-askpass-fullscreen
      zoom-us
    ];
    pathsToLink = [ "/share/zsh" ];
  };

  services = {
    dbus = { enable = true; };

    # trilium-server = {
    #   enable = true;
    #   noAuthentication = true;
    # };

    gnome = { gnome-keyring = { enable = true; }; };

    openldap = { enable = true; };

    vikunja = {
      enable = true;
      frontendScheme = "http";
      frontendHostname = "localhost";
      setupNginx = true;
    };
    nginx.enable = true;
  };

  users = { users = { freeman = { packages = with pkgs; [ tdesktop ]; }; }; };

  programs = {
    atop = {
      enable = true;
      netatop = { enable = true; };
      atopgpu = { enable = true; };
    };
    nm-applet = { enable = true; };
    nix-ld.enable = true;
  };

  home-manager = { users = { freeman = { imports = [ ../home/gui ]; }; }; };

  nix = {
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      restrict-eval = false
      sandbox = false
    '';
  };
}

