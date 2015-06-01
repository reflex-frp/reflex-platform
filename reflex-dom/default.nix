{ fetchUrl, overrideCabal, callPackage, reflex-dom }:

let
  rev = "ec3e459e69113402ba6edf860e1d20670c5452be";
  sha256 = "016dgjm0m3m82z5z4s771dwl64fsbri4viawc7qhzi2jmq2nfjhb";
  masterDefaultNix = fetchUrl {
    url = "https://raw.githubusercontent.com/ryantrinkle/reflex-dom/master/default.nix";
    inherit sha256;
  };
  expr = callPackage masterDefaultNix {};
in overrideCabal (drv: { inherit (expr) buildDepends; })
