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
      appimage-run
      awscli2
      cachix
      cargo
      deadnix
      delve
      dig
      discord
      dive # docker image analyzer
      du-dust
      fd
      firefox
      fluxcd
      git-crypt
      gitAndTools.gitflow
      gitleaks
      gitops
      gopls
      graphviz
      grpcurl
      gotron-sdk
      (kubernetes-helm-wrapped.override {
        plugins = [ kubernetes-helmPlugins.helm-diff ];
      })
      jetbrains.idea-ultimate
      jdt-language-server
      kube-capacity
      kube-prompt
      kubectl
      kubectl-tree
      kubespy
      kubeshark
      krew
      kconf
      kubelogin-oidc
      openlens
      litecli
      maven
      mattermost-desktop
      microsoft-edge-dev
      mongosh
      mycli
      neofetch
      nix-du
      nixfmt
      nix-index-update
      node2nix
      nodejs_latest
      nodePackages."bash-language-server"
      nodePackages."prettier"
      nodePackages."typescript-language-server"
      nodePackages."yaml-language-server"
      nodePackages."vscode-langservers-extracted"
      nvfetcher
      oath-toolkit
      openai
      openssl
      pulumi-bin
      pulumiPackages.pulumi-aws-native
      pulumiPackages.pulumi-language-go
      pgadmin4-desktopmode
      pgcli
      plantuml
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
      qrencode
      ripgrep
      rust-analyzer
      rustscan
      rustc
      sbcl
      scalafmt
      shellcheck
      shfmt
      slack
      solc-select
      solium
      sops
      socat
      statix
      stow
      tectonic
      terraform
      terraform-ls
      terracognita
      terranix
      terraformer
      tf2pulumi
      tealdeer
      universal-ctags
      unzip
      virtualenv
      wakatime
      wget
      wpsoffice
      yarn2nix
      zookeeper
      zoom-us
    ];
  };
}
