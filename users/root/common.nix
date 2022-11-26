# Edit this configuration file to define what should be installed on
{
  hmUsers,
  pkgs,
  ...
}: {
  users.users.root = {
    shell = pkgs.zsh;
  };
  home-manager.users = {inherit (hmUsers) root;};

  # home-manager.users = {
  #   root = {
  #     home = {
  #       stateVersion = "22.11";
  #       keyboard = {options = ["caps:ctrl_modifier"];};
  #       file = let
  #         old-files-path = ../old-files;
  #       in {
  #         ".wakatime.cfg" = {source = old-files-path + /wakatime/.wakatime.cfg;};
  #         ".ldaprc" = {source = old-files-path + /ldap/.ldaprc;};
  #         ".curlrc" = {source = old-files-path + /downloader/.curlrc;};
  #         ".ssh/id_ed25519.pub" = {
  #           text = profiles.share.office.user.public-key;
  #           executable = false;
  #         };
  #         ".ssh/id_ed25519" = {
  #           source = ../secrets/office_pk.key;
  #           executable = false;
  #         };
  #       };
  #     };
  #   };
  # };
}
