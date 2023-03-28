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
      ec2_ami_tools
      ec2-api-tools
      awscli2
      cachix
      deploy-rs
      discord
      delve
      dig
      deadnix
      du-dust
      fd
      foundry-bin
      firefox
      kompose
      tealdeer
      terraform-ls
      terraform-lsp
      pulumi
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
      kubernetes-helm
      litecli
      # metals
      kubectl
      marksman
      pgadmin4-desktopmode
      mongosh
      mycli
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
