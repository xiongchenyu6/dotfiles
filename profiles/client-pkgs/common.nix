# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, ... }: {
  nix = {
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
  };
  environment = {
    systemPackages = with pkgs; [
      appimage-run
      awscli2
      bundix
      cachix
      cargo
      deadnix
      delve
      dig
      discord
      dive # docker image analyzer
      du-dust
      datree
      easyrsa
      fd
      ffmpeg-full
      fluxcd
      glab
      git-crypt
      gitAndTools.gitflow
      gitleaks
      weave-gitops
      gopls
      graphviz
      grpcurl
      gotron-sdk
      (kubernetes-helm-wrapped.override {
        plugins = [ kubernetes-helmPlugins.helm-diff ];
      })
      helmify
      hey
      jetbrains.idea-ultimate
      jdt-language-server
      kots
      kube-capacity
      kube-prompt
      kubectl
      kubectl-tree
      kubespy
      kubeshark
      kustomize
      krew
      kconf
      kubelogin-oidc
      kubevirt
      kube-score
      killall
      openlens
      linkerd
      litecli
      maven
      mattermost-desktop
      microsoft-edge-dev
      mongosh
      mycli
      my2sql
      neofetch
      xournal
      nix-du
      nixfmt
      nix-index-update
      nixpacks
      nixd
      node2nix
      nodejs_latest
      nodePackages."bash-language-server"
      nodePackages."prettier"
      nodePackages."typescript-language-server"
      nodePackages."yaml-language-server"
      nodePackages."vscode-langservers-extracted"
      nodePackages."yarn"
      # nvfetcher
      oath-toolkit
      openai
      # openssl
      pg-ldap-sync
      popeye
      pulumi-bin
      pulumiPackages.pulumi-aws-native
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
      redis
      rust-analyzer
      rustscan
      rustc
      ruby
      sbcl
      scalafmt
      shellcheck
      shfmt
      slack
      s3cmd
      solc-select
      solium
      sops
      socat
      statix
      stow
      sshpass
      step-cli
      spring-boot-cli
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
      # yubikey-manager
      zookeeper
      zoom-us
    ];
  };
}
