# lubancat — an aarch64 SBC running Ubuntu, not NixOS. Nix is installed, so
# home-manager manages the shell environment standalone.
#
# Deliberately does not import home-modules/default.nix (importDefault is off
# for this user in flake.nix): that aggregate wires sops-nix to dotfiles'
# secrets/common.yaml, which is encrypted to host age keys this machine does not
# have, so activation would fail on decryption.
{
  pkgs,
  ezModules,
  ...
}:
{
  imports = [
    # zsh-minimal is what actually sets programs.zsh.enable; zsh.nix only layers
    # antidote plugins on top. The aggregate in home-modules/default.nix
    # normally pulls the former in, and that is skipped here.
    ezModules.zsh-minimal
    ezModules.zsh
    ezModules.cli-server
    ezModules.tmux
  ];

  home = {
    username = "cat";
    homeDirectory = "/home/cat";
    stateVersion = "24.05";

    # Ubuntu's own binaries stay on PATH; these are additions, not a takeover.
    packages = with pkgs; [
      dua
      duf
    ];
  };

  programs.home-manager.enable = true;
}
