# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, ... }:
{
  nix = {
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
  };
  environment = {
    systemPackages = with pkgs; [
      awscli2
      cachix
      deadnix
      delve # go debugger
      dig
      dive # docker image analyzer
      du-dust # du alternative
      envsubst
      fd
      ffmpeg-full
      fluxcd
      glab
      gitAndTools.gitflow
      github-copilot-cli
      git-standup
      gitleaks
      weave-gitops
      graphviz
      grpcurl
      #gotron-sdk
      (kubernetes-helm-wrapped.override { plugins = [ kubernetes-helmPlugins.helm-diff ]; })
      #helmify
      hey # http load generator
      kube-capacity
      kube-prompt
      kubectl
      kubectl-tree
      kubespy
      kubeshark
      kustomize
      krew
      kconf
      #korb
      kubelogin-oidc
      kube-score
      killall
      litecli
      vsc-leetcode-cli
      mongosh
      mycli
      my2sql
      neofetch
      # nix-du
      nix-index-update
      nixpacks
      nixd
      bun
      ngrok
      nvfetcher
      oath-toolkit
      openssl
      #pg-ldap-sync
      popeye
      pgcli
      plantuml
      (python3.withPackages (
        _: with python3.pkgs; [
          pip
        ]
      ))
      qrencode
      ripgrep
      redis
      rustscan
      ruby
      shellcheck
      shfmt
      s3cmd
      solc-select
      #solium
      sops
      socat
      stow
      sshpass
      spring-boot-cli
      tgpt
      # terraform
      # terraform-ls
      # terracognita
      # terranix
      # terraformer
      # tf2pulumi
      tealdeer
      unzip
      wakatime
      wget
      yarn2nix
      #yubikey-manager
    ];
  };
}
