_: prev: {
  hydra-unstable = prev.hydra-unstable.overrideAttrs (_: {doCheck = false;});
}
