{ pkgs }:

with pkgs.lib; {
  # path -> [path]
  ls = dir: builtins.map (f: (dir + "/${f}")) (builtins.attrNames (builtins.readDir dir));

  # Add your library functions here
  #
  # hexint = x: hexvals.${toLower x};
}
