{ profiles, ... }: {
  users.mutableUsers = false;

  users.users.root = {
    openssh.authorizedKeys.keys =
      [ profiles.share.users-dict."freeman.xiong".pk ];

  };
  imports = [ ./common.nix ];
}
