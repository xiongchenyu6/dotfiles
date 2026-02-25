{ pkgs, lib, ... }:
{
  wsl = {
    enable = true;
    defaultUser = lib.mkDefault "freeman.xiong";
    extraBin = with pkgs; [
      { src = "${coreutils}/bin/uname"; }
      { src = "${coreutils}/bin/dirname"; }
      { src = "${coreutils}/bin/readlink"; }
    ];
  };

  environment.systemPackages = with pkgs; [ wslu ];
}
