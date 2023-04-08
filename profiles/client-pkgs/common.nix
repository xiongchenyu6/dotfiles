# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, ... }: {
  nix = {
    generateNixPathFromInputs = true;
    generateRegistryFromInputs = true;
    linkInputs = true;
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
  };

  environment = {
    systemPackages = with pkgs; [
      # albert
      aiac
      appimage-run
      awscli2
      cachix
      cargo
      colmena
      discord
      delve
      dig
      deadnix
      du-dust
      fd
      foundry-bin
      firefox
      jetbrains.idea-ultimate
      jdt-language-server
      kompose
      tealdeer
      wpsoffice
      terraform-ls
      terraform-lsp
      tf2pulumi
      pulumi-bin
      pulumiPackages.pulumi-aws-native
      pulumiPackages.pulumi-language-go
      socat
      gotron-sdk
      rustscan
      gitAndTools.gitflow
      git-crypt
      gopls
      graphviz
      gptcommit
      grpcurl
      (python3.withPackages (_:
        with python3.pkgs; [
          cmake-language-server
          colour
          # epc
          ipython
          matplotlib
          nbformat
          newsapi-python
          nltk
          orjson
          python-lsp-server
          pandas
          python-dotenv
          six
          virtualenv
          openai
        ]))
      (kubernetes-helm-wrapped.override {
        plugins = [ kubernetes-helmPlugins.helm-diff ];
      })
      helmfile
      litecli
      # metals
      kubectl
      kubelogin-oidc
      marksman
      microsoft-edge-dev
      pgadmin4-desktopmode
      mongosh
      mycli
      maven
      nixfmt
      nix-du
      neofetch
      node2nix
      nodejs_latest
      nodePackages."bash-language-server"
      nodePackages."prettier"
      nodePackages."typescript-language-server"
      nodePackages."yaml-language-server"
      nodePackages."vscode-langservers-extracted"
      nvfetcher
      nix-index-update
      nil
      openssl
      oath-toolkit
      pgcli
      plantuml
      qrencode
      ripgrep
      terraform
      terranix
      tectonic
      terracognita
      terraformer
      #texlive.combined.scheme-full
      tronbox
      unzip
      universal-ctags
      rust-analyzer
      rustc
      sbcl
      slack
      statix
      shellcheck
      solium
      shfmt
      sops
      scalafmt
      solc-select
      stow
      yarn2nix
      # winklink
      wakatime
      zoom-us
    ];
  };
}
