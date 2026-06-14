{
  lib,
  ...
}:

{
  # Home Manager's built-in `targets.darwin.copyApps` now handles Spotlight
  # discovery. Remove the legacy app copy directory to avoid duplicate
  # Cmd+Space results next to "Home Manager Apps".
  home.activation = {
    removeLegacyNixApps = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
      rm -rf "$HOME/Applications/Nix Apps"
    '';
  };
}
