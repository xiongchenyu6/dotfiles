{
  # path -> [path]
  ls = dir: builtins.map (f: (dir + "/${f}")) (builtins.attrNames (builtins.readDir dir));

  # Add your library functions herel
  
  
  #
  # hexint = x: hexvals.${toLower x};
  builtins.readFile = path: builtins.fromJSON (builtins.readFile path);  
}
