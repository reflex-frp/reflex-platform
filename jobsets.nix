{ prs }:

let
  pkgs = import ./nixpkgs {};
  mkFetchGithub = value: {
    inherit value;
    type = "git";
    emailresponsible = false;
  };
in
with pkgs.lib;
let
  defaults = jobs: {
    inherit (jobs) description;
    enabled = 1;
    hidden = false;
    keepnr = 10;
    schedulingshares = 100;
    checkinterval = 120;
    enableemail = false;
    emailoverride = "";
    nixexprinput = "reflex-platform";
    nixexprpath = "release.nix";
    inputs = jobs.inputs // {
      nixpkgs = {
        type = "git";
        value = "https://github.com/NixOS/nixpkgs-channels nixos-unstable";
        emailresponsible = false;
      };
    };
  };
  branchJobset = branch: defaults {
    description = "reflex-platform-${branch}";
    inputs = {
      reflex-platform = {
        value = "https://github.com/reflex-frp/reflex-platform ${branch}";
        type = "git";
        emailresponsible = false;
      };
    };
  };
  makePr = num: info: {
    name = "reflex-platform-pr-${num}";
    value = defaults {
      description = "#${num}: ${info.title}";
      inputs = {
        reflex-platform = {
          #NOTE: This should really use "pull/${num}/merge"; however, GitHub's
          #status checks only operate on PR heads.  This creates a race
          #condition, which can currently only be solved by requiring PRs to be
          #up to date before they're merged.  See
          #https://github.com/isaacs/github/issues/1002
          value = "https://github.com/reflex-frp/reflex-platform pull/${num}/head";
          type = "git";
          emailresponsible = false;
        };
      };
    };
  };
  processedPrs = mapAttrs' makePr (builtins.fromJSON (builtins.readFile prs));
  jobsetsAttrs = processedPrs //
    genAttrs ["develop"] branchJobset;
in {
  jobsets = pkgs.writeText "spec.json" (builtins.toJSON jobsetsAttrs);
}
