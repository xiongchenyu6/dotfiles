with import <nixpkgs> {}; 
let cryptobox = callPackage ./default.nix {};
in
pkgs.dockerTools.buildImage { 
  name = "nix-htop"; 
  contents = [cryptobox]; 
  config = { Cmd = [ "ls" ]; 
}; 
}
