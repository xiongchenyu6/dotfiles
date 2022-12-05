{
  virtualisation = {
    podman = {
      enable = true;
      dockerSocket.enable = true;
      defaultNetwork.dnsname.enable = true;
    };
    # oci-containers.containers = {
    #   "my-container" = {
    #     image = "grafana/oncall:latest";
    #     cmd = ["sleep" "infinity"];
    #     ports = ["8964:80"];
    #   };
    # };
  };

  networking.firewall.checkReversePath = false;
}
