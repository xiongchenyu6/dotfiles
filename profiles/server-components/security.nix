{
  security = {
    # sudo = {
    #   extraRules = [
    #     {
    #       groups = ["developers"];
    #       commands = ["ALL"];
    #     }
    #   ];
    # };
    pam = {
      makeHomeDir = { skelDirectory = "/etc/skel"; };
      services = {
        sshd = {
          makeHomeDir = true;
          sssdStrictAccess = true;
        };
      };
    };
  };
  environment = {
    etc = {
      "skel/.zshrc" = {
        mode = "0755";
        text = "";
      };
      "nsswitch.conf".text = ''
        sudoers: files sss
      '';
    };
  };
}
