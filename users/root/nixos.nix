{ profiles, lib, ... }: {

  users.mutableUsers = lib.mkDefault false;

  users.users.root = {
    openssh.authorizedKeys.keys =
      [ profiles.share.users-dict."freeman.xiong".public-key ];

  };
  imports = [ ./common.nix ];
}
