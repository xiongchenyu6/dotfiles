{
  services = {
    postgresql = {
      enable = false;
      authentication = ''
        local all all trust
      '';
    };
  };
}
