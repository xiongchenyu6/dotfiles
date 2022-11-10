# Edit this configuration file to define what should be installed on
{ profiles, ... }: {
  users.users.root = {
    openssh.authorizedKeys.keys = [ profiles.share.office.user.public-key ];
  };
}
