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
  haven = markUnbroken super.haven;

  # These take over an hour to run, each
  cryptonite = dontCheck super.cryptonite;
  scientific = dontCheck super.scientific;

  # pandoc 2.11 and dependencies
  commonmark = self.callHackage "commonmark" "0.1.0.2" {};
  commonmark-extensions = self.callHackage "commonmark-extensions" "0.2.0.1" {};
  commonmark-pandoc = self.callHackage "commonmark-pandoc" "0.2.0.1" {};
  citeproc = doJailbreak (self.callHackage "citeproc" "0.1.0.2" {});
  emojis = self.callHackage "emojis" "0.1" {};
  jira-wiki-markup = self.callHackage "jira-wiki-markup" "1.3.2" {};
  pandoc = self.callHackage "pandoc" "2.11.0.2" {};
  pandoc-types = self.callHackage "pandoc-types" "1.22" {};
  texmath = self.callHackage "texmath" "0.12.0.3" {};
  rfc5051 = self.callHackage "rfc5051" "0.2" {};
  HsYAML = self.callHackage "HsYAML" "0.2.1.0" {};
  connection = self.callHackage "connection" "0.3.1" {};
  doclayout = self.callHackage "doclayout" "0.3" {};
  doctemplates = self.callHackage "doctemplates" "0.8.2" {};
  haddock-library = doJailbreak (self.callHackage "haddock-library" "1.8.0" {});
  hslua  = self.callHackage "hslua" "1.1.0" {};
  skylighting = self.callHackage "skylighting" "0.10.0.2" {};
  skylighting-core = self.callHackage "skylighting-core" "0.10.0.2" {};

  # beam packages
  beam-core = self.callHackage "beam-core" "0.9.0.0" {};
  beam-migrate = self.callHackage "beam-migrate" "0.5.0.0" {};
  beam-postgres = haskellLib.dontCheck (self.callHackage "beam-postgres" "0.5.0.0" {});
  beam-automigrate = self.callHackage "beam-automigrate" "0.1.0.0" {};

  # hnix 0.12 and dependencies
  hnix = dontCheck
    (overrideCabal (self.callCabal2nix "hnix" (nixpkgs.hackGet ./hnix/hnix) {}) (drv: {
      librarySystemDepends = (drv.librarySystemDepends or []) ++ [ nixpkgs.nix ];
      testHaskellDepends = (drv.testHaskellDepends or []) ++ [ nixpkgs.nix super.criterion ];
    }));
  hnix-store-core = dontCheck
    (self.callCabal2nix "hnix" (nixpkgs.hackGet ./hnix/hnix-store + "/hnix-store-core") {});
  hnix-store-remote =
    self.callCabal2nix "hnix" (nixpkgs.hackGet ./hnix/hnix-store + "/hnix-store-remote") {};
  data-fix = self.callHackage "data-fix" "0.3.0" {};
  neat-interpolation = self.callHackage "neat-interpolation" "0.4" {};
  prettyprinter = self.callHackage "prettyprinter" "1.7.0" {};
  cryptohash-sha512 = doJailbreak super.cryptohash-sha512;
  ListLike = self.callHackage "ListLike" "4.7.3" {};

}
