{ pkgs ? (builtins.getFlake "nixpkgs").legacyPackages.x86_64-linux, pulls, ... }:

let

  prs = builtins.fromJSON (builtins.readFile pulls);
  prJobsets = pkgs.lib.mapAttrs'
    (num: info: {
      name = "PR-${num}";
      value = {
        enabled = 1;
        hidden = false;
        description = "PR ${num}: ${info.title}";
        checkinterval = 120;
        schedulingshares = 20;
        enableemail = true;
        emailoverride = "tom@example.com";
        keepnr = 1;
        type = 1;
        flake = "github:tomberek/hydra-demo/${info.head.ref}";
        enable_dynamic_run_command = true;
      };
    }
    )
    prs;
  mkFlakeJobset = branch: {
    description = "Hydra demo - ${branch}";
    checkinterval = 600;
    enabled = 1;
    schedulingshares = 100;
    enableemail = true;
    emailoverride = "tom@example.com";
    keepnr = 3;
    hidden = false;
    type = 1;
    flake = "github:tomberek/hydra-demo/${branch}";
    enable_dynamic_run_command = true;
  };

  desc = prJobsets // {
    "master" = mkFlakeJobset "master";
    "generative" = mkFlakeJobset "generative";
  };

  log = {
    pulls = prs;
    jobsets = desc;
  };

in
{
  jobsets = pkgs.runCommand "spec-jobsets.json" { } ''
    cat >$out <<EOF
    ${builtins.toJSON desc}
    EOF
    # This is to get nice .jobsets build logs on Hydra
    cat >tmp <<EOF
    ${builtins.toJSON log}
    EOF
    ${pkgs.jq}/bin/jq . tmp
  '';
}
