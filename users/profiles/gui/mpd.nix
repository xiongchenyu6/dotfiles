# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  services = {
    mpd = {
      enable = true;
      musicDirectory = "/home/freeman/Music/";
      extraConfig = ''
        audio_output {
                type            "pipewire"
                name            "PipeWire Sound Server"
        }
        filesystem_charset        "UTF-8"

        id3v1_encoding          "gb2312"
      '';
    };
  };
  programs = {
    ncmpcpp = {
      enable = true;
    };
  };
}
