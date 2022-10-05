{
  security = {
    sudo = {
      extraRules = [
        {
          groups = [ "developers" ];
          commands = [ "ALL" ];
        }
      ];
    };
    pam = {
      krb5.enable = false;
      services = {
        sshd = {
          makeHomeDir = true;
        };
      };
    };
  };
}
