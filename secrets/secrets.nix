let

  freeman = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIABVd0cIdwKzf4yLoRXQwjaaVYPFv8ZfYvTUMOMTFJ/p freeman@nixos";
  users = [ freeman ];

  laptop = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK9lhSffZNM3UYm884iQc/XmWL+g5fnePXUh4mPFkuNy root@nixos";
  tc = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKcBsPZi+OYEL/RCSGZMr82x0UGUaghP3AGl6M57ssjn";
  systems = [ laptop tc ];
in
{
  "freeman_wg_pk.age".publicKeys = [ freeman laptop ];
  "tc_wg_pk.age".publicKeys = [ freeman tc ];
}

