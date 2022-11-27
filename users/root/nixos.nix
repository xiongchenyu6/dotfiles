{profiles, ...}: {
  users.users.root = {
    openssh.authorizedKeys.keys = [profiles.share.office.user.public-key];
  };
  imports = [./common.nix];
}
