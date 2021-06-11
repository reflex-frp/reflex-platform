{ haskellLib
, fetchFromGitHub
, nixpkgs
}:
with haskellLib;
self: super: {

  # These take over an hour to run, each
  cryptonite = dontCheck super.cryptonite;
  scientific = dontCheck super.scientific;

}
