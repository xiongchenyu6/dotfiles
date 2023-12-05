{ pkgs, ... }: {
  wsl = {
    enable = true;
    defaultUser = "freeman.xiong";
    extraBin = with pkgs; [
      { src = "${coreutils}/bin/uname"; }
      { src = "${coreutils}/bin/dirname"; }
      { src = "${coreutils}/bin/readlink"; }
    ];
  };
  services = { vscode-server.enable = true; };

  environment.systemPackages = with pkgs; [ wslu ];

}
