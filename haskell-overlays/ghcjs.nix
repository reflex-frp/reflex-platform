{ haskellLib, nixpkgs, fetchFromGitHub, useReflexOptimizer, hackGet }:

with haskellLib;

self: super: {
  ghcWithPackages = selectFrom: self.callPackage (nixpkgs.path + "/pkgs/development/haskell-modules/with-packages-wrapper.nix") {
    inherit (self) llvmPackages;
#    haskellPackages = self;
    packages = selectFrom self;
    ${if useReflexOptimizer then "ghcLibdir" else null} = "${self.ghc.bootPackages.ghcWithPackages (p: [ p.reflex ])}/lib/${self.ghc.bootPackages.ghc.name}";
  };

  ghcjs-base = overrideCabal (self.callCabal2nix "ghcjs-base" (fetchFromGitHub {
    owner = "ghcjs";
    repo = "ghcjs-base";
    rev = "43804668a887903d27caada85693b16673283c57";
    sha256 = "1pqmgkan6xhpvsb64rh2zaxymxk4jg9c3hdxdb2cnn6jpx7jsl44";
  }) {}) (drv: {
    jailbreak = true;
    doCheck = false; #TODO: This should be unnecessary

    #TODO: This should be unnecessary
    preConfigure = (drv.preConfigure or "") + ''
      sed -i -e '/jsbits\/export.js/d' -e '/GHCJS\.Foreign\.Export/d' *.cabal
    '';
  });

  ghc = super.ghc // {
    withPackages = self.ghcWithPackages;
  };

  diagrams-lib = dontCheck super.diagrams-lib;
  linear = dontCheck super.linear;
  bytes = dontCheck super.bytes;

  hlint = null;
  hscolour = null;
  cabal-macosx = null;

  # installHandler: not available for GHCJS
  tasty-quickcheck = dontCheck super.tasty-quickcheck;
  blaze-markup = dontCheck super.blaze-markup;
  scientific = dontCheck super.scientific;
  cookie = dontCheck super.cookie;
  uuid-types = dontCheck super.uuid-types;
  cryptohash-sha1 = dontCheck super.cryptohash-sha1;
  cryptohash-md5 = dontCheck super.cryptohash-md5;
  http-types = dontCheck super.http-types;
  wai = dontCheck super.wai;
  fast-logger = dontCheck super.fast-logger;
  iproute = dontCheck super.iproute;
  logging-facade = dontCheck super.logging-facade;
  streaming-commons = dontCheck super.streaming-commons;
  memory = dontCheck super.memory;
  aeson = dontCheck super.aeson;
  these = dontCheck super.these;

  hspec-core = dontCheck super.hspec-core;
  hspec-discover = dontCheck super.hspec-discover;
  hspec-meta-discover = dontCheck super.hspec-meta-discover;
  hspec = dontCheck super.hspec;
  bifunctors = dontCheck super.bifunctors;
  base-compat = dontCheck super.base-compat;
  generic-deriving = dontCheck super.generic-deriving;
  newtype-generics = dontCheck super.newtype-generics;
  lens = disableCabalFlag (dontCheck super.lens) "test-properties";
  word8 = dontCheck super.word8;
  mockery = dontCheck super.mockery;
  http2 = dontCheck super.http2;
  wai-extra = dontCheck super.wai-extra;

  #TODO: These seem like they might be real failures
  Glob = dontCheck super.Glob;

  #TODO: Do we need this patch?
  # hashable = appendPatch super.hashable ../hashable-1.2.6.1.patch;

  # doctest doesn't work on ghcjs, but sometimes dontCheck doesn't seem to get rid of the dependency
  doctest = builtins.trace "Warning: ignoring dependency on doctest" null;
}
