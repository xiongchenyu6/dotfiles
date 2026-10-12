# macOS GUI services — fonts, Homebrew, PostgreSQL
{ config, pkgs, ... }:
{
  # `mas` (App Store installs run by `brew bundle` during activation) calls
  # sudo for every install and has no terminal to ask for a password.
  security.sudo.extraConfig = ''
    ${config.system.primaryUser} ALL=(root) NOPASSWD: /opt/homebrew/bin/mas
  '';

  fonts = {
    packages = with pkgs; [
      nerd-fonts.hack
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-cjk-serif
    ];
  };

  # GUI apps, in order of preference: Mac App Store (masApps) when the app is
  # there, else a Homebrew cask, never nixpkgs. Both track upstream releases
  # faster than nixpkgs and avoid duplicating app bundles in /nix/store. The
  # Nix side stays the dev environment (CLI, toolchains, fonts).
  homebrew = {
    enable = true;
    brews = [ "mas" ]; # App Store CLI used by masApps
    masApps = {
      Slack = 803453959;
      Telegram = 747648890;
    };
    casks = [
      "google-chrome"
      "iterm2"
      "squirrel" # Rime input method; schema data in stow-managed/rime
      "bitwarden"
      "keepassxc"
      "discord"
      "zoom"
      "lark"
      "zotero"
      "rustdesk"
      "chatgpt"
    ];
    global = {
      autoUpdate = true;
      brewfile = true;
    };
    onActivation = {
      autoUpdate = true;
      cleanup = "zap";
      upgrade = true;
    };
  };

  services = {
    postgresql = {
      enable = true;
      package = pkgs.postgresql;
      enableTCPIP = true;
    };
  };
}
