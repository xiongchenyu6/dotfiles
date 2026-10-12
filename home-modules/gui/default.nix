# GUI module entry point: cross-platform packages plus per-platform config.
# linux.nix and darwin.nix use mkIf internally for platform gating.
{
  imports = [
    ./packages.nix
    ./linux.nix
    ./darwin.nix
  ];
}
