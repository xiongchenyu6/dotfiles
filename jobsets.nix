{ pkgs ? import <nixpkgs> { } }:

with pkgs;
let
  mkJobsets = drv { inherit lib writeText; };
  drv = { lib, writeText }: { owner, repo, branches, pullRequests }:
    let
      toJobset = { url, ref }: {
        checkinterval = 100;
        emailoverride = "";
        enabled = 1;
        enableemail = false;
        hidden = false;
        type = 1;
        keepnr = 10;
        description = "branch-${ref}";
        schedulingshares = 100;
        flake = "github:tomberek/hydra-demo";
      };

      branchToJobset = ref: toJobset {
        url = "https://github.com/${owner}/${repo}.git";
        inherit ref;
      };

      pullRequestToJobset = n: pr: toJobset {
        url = "https://github.com/${pr.base.repo.owner.login}/${pr.base.repo.name}.git";
        ref = "pull/${n}/head";
      };

      jobsetsAttrs =
        lib.mapAttrs pullRequestToJobset (lib.importJSON pullRequests) // lib.genAttrs branches branchToJobset;
    in
    {
      jobsets = writeText "jobsets.json" (builtins.toJSON jobsetsAttrs);
    };

in

mkJobsets {
  owner = "tomberek";
  repo = "hydra-demo";
  branches = [ "master" ];
  pullRequests = <pull-requests>;
}
