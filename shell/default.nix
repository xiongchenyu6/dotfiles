_: {
  exportedModules = { pkgs, lib, inputs, extraModulesPath, ... }:
    let
      inherit (pkgs)
        sops cachix editorconfig-checker mdbook nixUnstable nixfmt statix
        nvfetcher # nix-linter
        ssh-to-age;
      pkgWithCategory = category: package: { inherit package category; };
      devos = pkgWithCategory "devos";
      linter = pkgWithCategory "linter";
      docs = pkgWithCategory "docs";
    in {
      imports = [
        "${extraModulesPath}/git/hooks.nix"
        "${extraModulesPath}/services/postgres.nix"
      ];
      git = {
        hooks = {
          enable = true;
          pre-commit.text = ''
            echo "commit hook"
            nix build .#checks.${pkgs.system}.pre-commit-check
          '';
        };
      };
      env = [{
        name = "NIX_SSHOPTS";
        value = "-Y -p 2222";
      }];

      commands = [
        (devos nixUnstable)
        (devos sops)
        (devos ssh-to-age)
        (devos nvfetcher)
        (linter nixfmt)
        (linter statix)
        (linter editorconfig-checker)
        # (linter nix-linter)
        (docs mdbook)
      ] ++ lib.optionals (!pkgs.stdenv.buildPlatform.isi686) [ (devos cachix) ]
        ++ lib.optionals (pkgs.stdenv.hostPlatform.isLinux
          && !pkgs.stdenv.buildPlatform.isDarwin) [
            (devos inputs.nixos-generators.defaultPackage.${pkgs.system})
            (devos inputs.deploy-rs.packages.${pkgs.system}.deploy-rs)
          ];
      services = { postgres = { setupPostgresOnStartup = false; }; };
    };
}
