# GUI module entry point: imports cross-platform packages, plus Linux-only config on Linux
# linux.nix uses mkIf internally for platform gating
{
  imports = [
    ./packages.nix
    ./linux.nix
  ];
}
