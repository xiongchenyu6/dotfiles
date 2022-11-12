{pkgs, ...}: {
  services = {
    getty = {
      autologinUser = "freeman";
    };
  };
  environment = {
    systemPackages = with pkgs; [
      wofi
      grim
      slurp
      brightnessctl
    ];
  };
}
