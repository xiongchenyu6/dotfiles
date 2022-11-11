_: {
  exportedModules = {
    pkgs,
    lib,
    inputs,
    extraModulesPath,
    ...
  }: let
    inherit
      (pkgs)
      sops
      cachix
      editorconfig-checker
      mdbook
      nixUnstable
      nixfmt
      nvfetcher
      ;
    pkgWithCategory = category: package: {inherit package category;};
    devos = pkgWithCategory "devos";
    linter = pkgWithCategory "linter";
    docs = pkgWithCategory "docs";
  in {
    imports = ["${extraModulesPath}/git/hooks.nix"];
    git = {
      hooks = {
        enable = true;
        pre-commit.text = ''
          echo "commit hook"
          system=$(uname -s | tr '[:upper:]' '[:lower:]')
          arch=$(uname -m)
          nix build .#checks.$arch-$system.pre-commit-check
        '';
      };
    };

    commands =
      [
        (devos nixUnstable)
        (devos sops)
        (devos nvfetcher)
        (linter nixfmt)
        (linter editorconfig-checker)

        (docs mdbook)
      ]
      ++ lib.optionals (!pkgs.stdenv.buildPlatform.isi686) [(devos cachix)]
      ++ lib.optionals (pkgs.stdenv.hostPlatform.isLinux
        && !pkgs.stdenv.buildPlatform.isDarwin) [
        (devos inputs.nixos-generators.defaultPackage.${pkgs.system})
        (devos inputs.deploy-rs.packages.${pkgs.system}.deploy-rs)
      ];
  };
}
