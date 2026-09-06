{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # Disk imaging and diagnostics
    ddrescue
    smartmontools
    hdparm
    util-linux
    coreutils

    # Filesystem access, forensics, and recovery
    ntfs3g
    sleuthkit
    testdisk # Includes PhotoRec
    autopsy
  ];
}
