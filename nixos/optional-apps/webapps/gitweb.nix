{
  services = { nginx = { gitweb = { enable = true; }; }; };
  gitweb = {
    projectroot = "/tmp/test";
    gitwebTheme = true;
  };
}

