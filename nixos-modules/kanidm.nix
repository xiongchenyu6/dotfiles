{
  pkgs,
  config,
  ...
}:
{
  services = {
    kanidm =
      let
        credsDir = config.security.acme.certs."${config.networking.domain}".directory;
      in
      {
        enableServer = true;
        enablePam = true;
        clientSettings = {
          uri = "https://kanidm.auto-life.tech";
        };
        unixSettings = {
          default_shell = "${pkgs.zsh}/bin/zsh";
          home_alias = "name";
          home_attr = "uuid";
          pam_allowed_login_groups = [ "devops" ]; # Updated group to match changes in groups
        };

        serverSettings = {
          tls_key = credsDir + "/key.pem";
          tls_chain = credsDir + "/cert.pem";
          origin = "https://kanidm.auto-life.tech";
          domain = "kanidm.auto-life.tech";
          bindaddress = "0.0.0.0:443";
          online_backup.versions = 3;
        };
        provision = {
          enable = true;
          groups = {
            devops = {
              members = [
                "xiongchenyu"
                "chensiwei"
                "benjaming"
                "yongyigan"
                "huxiaoxiang" # Added huxiaoxiang to the devops group
                "liwenkai" # Added liwenkai to the devops group
              ];
            };
          };
          persons = {
            xiongchenyu = {
              mailAddresses = [ "xiongchenyu6@gmail.com" ];
              legalName = "Xiong Chenyu";
              displayName = "Xiong Chenyu";
              groups = [ "devops" ]; # Updated group to match changes in groups
            };
            chensiwei = {
              mailAddresses = [ "chensiwei@autolife.ai" ];
              legalName = "Chen Siwei";
              displayName = "Chen Siwei";
              groups = [ "devops" ]; # Updated group to match changes in groups
            };
            benjaming = {
              mailAddresses = [ "benjaming@autolife.ai" ]; # Updated email address
              legalName = "Benjaming"; # Updated legal name
              displayName = "Benjaming"; # Updated display name
              groups = [ "devops" ]; # Updated group to match changes in groups
            };
            yongyigan = {
              mailAddresses = [ "yongyigan@gmail.com" ]; # Updated email address
              legalName = "Yong Yigan"; # Updated legal name
              displayName = "Yong Yigan"; # Updated display name
              groups = [ "devops" ]; # Updated group to match changes in groups
            };
            huxiaoxiang = {
              mailAddresses = [ "xiaoxiang.hu@autolife.ai" ]; # Added email address for huxiaoxiang
              legalName = "Huxiaoxiang"; # Added legal name for huxiaoxiang
              displayName = "Huxiaoxiang"; # Added display name for huxiaoxiang
              groups = [ "devops" ]; # Added group for huxiaoxiang
            };
            liwenkai = {
              mailAddresses = [ "liwenkai@example.com" ]; # Added email address for liwenkai
              legalName = "Liwenkai"; # Added legal name for liwenkai
              displayName = "Liwenkai"; # Added display name for liwenkai
              groups = [ "devops" ]; # Added group for liwenkai
            };
          };
          systems = {
            oauth2 = {
              robot-management-system = {
                public = true;
                enableLocalhostRedirects = true;
                preferShortUsername = true;
                displayName = "Robot Management System";
                originLanding = "https://robot-management-system.autolife-robotics.me/";
                originUrl = "https://robot-management-system.autolife-robotics.me/callback";
                scopeMaps = {
                  devops = [
                    "openid"
                    "profile"
                    "email"
                    "groups"
                  ];
                };
              };
            };
          };
        };
      };
  };
}
