{ haskellLib
, fetchFromGitHub
, nixpkgs
}:

with haskellLib;

self: super: {

  # Need an older version for GHC 8.6
  haddock-api = dontHaddock (doJailbreak (self.callHackage "haddock-api" "2.22.0" {}));
  # TODO this conflicts with the pandoc version
  # haddock-library = doJailbreak (self.callHackage "haddock-library" "1.7.0" {});

  # Fixing things that are marked broken in 20.09:
  constrained-dynamic = dontCheck (markUnbroken super.constrained-dynamic);
  haven = doJailbreak (markUnbroken super.haven);

  # These take over an hour to run, each
  cryptonite = dontCheck super.cryptonite;
  scientific = dontCheck super.scientific;

  # pandoc 2.10.1 and dependencies
  pandoc = dontCheck (self.callHackage "pandoc" "2.10.1" {});
  pandoc-types = self.callHackage "pandoc-types" "1.21" {};
  hslua = self.callHackage "hslua" "1.1.2" {};
  hslua-module-text = self.callHackage "hslua-module-text" "0.2.1" {};
  doctemplates = self.callHackage "doctemplates" "0.8.2" {};
  skylighting = self.callHackage "skylighting" "0.8.5" {};
  skylighting-core = self.callHackage "skylighting-core" "0.8.5" {};

  # beam packages
  beam-core = self.callHackage "beam-core" "0.9.1.0" {};
  beam-migrate = self.callHackage "beam-migrate" "0.5.1.0" {};
  beam-postgres = haskellLib.dontCheck (self.callHackage "beam-postgres" "0.5.1.0" {});
  beam-automigrate = self.callHackage "beam-automigrate" "0.1.2.0" {};

  # hnix 0.12 and dependencies
  hnix = dontCheck
    (overrideCabal (self.callCabal2nix "hnix" (nixpkgs.hackGet ./hnix/hnix) {}) (drv: {
      librarySystemDepends = (drv.librarySystemDepends or []) ++ [ nixpkgs.nix ];
      testHaskellDepends = (drv.testHaskellDepends or []) ++ [ nixpkgs.nix super.criterion ];
    }));
  hnix-store-core =
    self.callCabal2nix "hnix" (nixpkgs.hackGet ./hnix/hnix-store + "/hnix-store-core") {};
  hnix-store-remote =
    self.callCabal2nix "hnix" (nixpkgs.hackGet ./hnix/hnix-store + "/hnix-store-remote") {};
  ref-tf = dontCheck (self.callHackage "ref-tf" "0.4.0.2" {});
  data-fix = self.callHackage "data-fix" "0.3.0" {};
  prettyprinter = self.callHackage "prettyprinter" "1.7.0" {};
  cryptohash-sha512 = doJailbreak super.cryptohash-sha512;
  ListLike = self.callHackage "ListLike" "4.7.3" {};

  # ghcjs-promise is marked broken in nixpkgs
  ghcjs-promise = self.callHackage "ghcjs-promise" "0.1.0.3" {};

  utf8-string = self.callHackage "utf8-string" "1.0.1.1" {};

  modern-uri = doJailbreak super.modern-uri;
  http-link-header = doJailbreak super.http-link-header;
  vector-binary-instances = self.callHackage "vector-binary-instances" "0.2.5.1" {};

}
