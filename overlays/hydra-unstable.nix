final: prev: {
  hydra-unstable =
    prev.hydra-unstable.overrideAttrs (old: { doCheck = false; });
}
