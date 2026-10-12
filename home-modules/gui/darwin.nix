# macOS-only GUI config. Squirrel (brew cask) reads ~/Library/Rime: the schema
# data comes from the same rime-frost package as fcitx5-rime on Linux, the
# personal *.custom.yaml patches are linked live from stow-managed/rime, and
# the directory itself stays writable for build/, sync/ and user dictionaries.
{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
let
  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
  rimeData = "${pkgs.rime-frost}/share/rime-data";
  # Names come from the flake input (plain path, no build at eval time); the
  # package drops docs/licence/others, so skip those here too.
  rimeEntries = lib.filterAttrs (
    name: _:
    !(lib.hasSuffix ".md" name || lib.hasPrefix "." name || name == "LICENSE" || name == "others")
  ) (builtins.readDir inputs.rime-frost);
  schemaFiles = lib.mapAttrs' (
    name: _: lib.nameValuePair "Library/Rime/${name}" { source = "${rimeData}/${name}"; }
  ) rimeEntries;
  customDir = "${config.home.homeDirectory}/dotfiles/stow-managed/rime/.local/share/fcitx5/rime";
  customFiles = lib.genAttrs' [ "default.custom.yaml" "rime_frost.custom.yaml" "custom_phrase.txt" ] (
    name:
    lib.nameValuePair "Library/Rime/${name}" {
      source = config.lib.file.mkOutOfStoreSymlink "${customDir}/${name}";
    }
  );
in
{
  config = lib.mkIf isDarwin {
    home.file =
      schemaFiles
      // customFiles
      // {
        "Library/Rime/wanxiang-lts-zh-hans.gram".source = "${rimeData}/wanxiang-lts-zh-hans.gram";
      };
  };
}
