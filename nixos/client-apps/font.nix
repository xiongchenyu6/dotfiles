{ config, pkgs, lib, symlinkJoin, ... }:
{
  fonts = {
    fontconfig = { enable = true; };
    fontDir = { enable = true; };
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      wqy_microhei
      wqy_zenhei
      (nerdfonts.override { fonts = [ "Hack" ]; })
      jetbrains-mono
    ];
  };
}
