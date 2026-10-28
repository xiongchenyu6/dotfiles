{ ... }:
{
  # Follow the lockfile merged by the dependency-update CI, rather than
  # independently selecting untested inputs on the server.
  system.autoUpgrade = {
    enable = true;
    flake = "github:xiongchenyu6/dotfiles/main#oracle-arm-002";
    upgrade = false;
    operation = "switch";
    dates = "*-*-* *:20:00";
    randomizedDelaySec = "10min";
    persistent = true;
    allowReboot = false;
    flags = [
      "--use-substitutes"
      "--impure"
      "--max-jobs"
      "2"
    ];
  };
}
