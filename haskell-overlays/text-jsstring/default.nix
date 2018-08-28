{ lib, haskellLib, fetchFromGitHub, hackGet, fetchpatch }:

with lib;
with haskellLib;

self: super: {
  # text = (doCheck (self.callCabal2nix "text" (fetchFromGitHub {
  #   owner = "obsidiansystems";
  #   repo = "text";
  #   rev = "50076be0262203f0d2afdd0b190a341878a08e21";
  #   sha256 = "1vy7a81b1vcbfhv7l3m7p4hx365ss13mzbzkjn9751bn4n7x2ydd";
  # }) {})).overrideScope (self: super: {
  #   text = null;
  #   QuickCheck = haskellLib.addBuildDepend (self.callHackage "QuickCheck" "2.9.2" {}) self.tf-random;
  # });
  # parsec = dontCheck (self.callHackage "parsec" "3.1.13.0" {});
  mkDerivation = attrs: super.mkDerivation (attrs // {
    configureFlags = (attrs.configureFlags or []) ++ [
      "--ghcjs-option=-fno-full-laziness"
      "--ghcjs-option=-fno-enable-rewrite-rules"
    ];
  });
  jsaddle = overrideCabal super.jsaddle (drv: {
    buildDepends = (drv.buildDepends or []) ++ [
      self.ghcjs-base
      self.ghcjs-prim
    ];
  });
  attoparsec = overrideCabal super.attoparsec (drv: {
    src = fetchFromGitHub {
      owner = "luigy";
      repo = "attoparsec";
      rev = "e766a754811042f061b6b4498137d2ad28e207a8";
      sha256 = "106fn187hw9z3bidbkp7r4wafmhk7g2iv2k0hybirv63f8727x3x";
    };
  });
  buffer-builder = overrideCabal super.buffer-builder (drv: {
    doCheck = false;
    src = fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "buffer-builder";
      rev = "59c730e0dec7ff0efd8068250f4bca9cb74c471d";
      sha256 = "18dd2ydva3hnsfyrzmi3y3r41g2l4r0kfijaan85y6rc507k6x5c";
    };
  });
  hashable = overrideCabal super.hashable (drv: {
    revision = null;
    editedCabalFile = null;
    jailbreak = true;
    doCheck = false;
    libraryHaskellDepends = (drv.libraryHaskellDepends or []) ++ [
      self.text
    ];
    patches = (drv.patches or []) ++ [
      ./hashable.patch
    ];
  });
  conduit-extra = dontCheck (appendPatch super.conduit-extra ./conduit-extra-text-jsstring.patch);
  double-conversion = overrideCabal super.double-conversion (drv: {
    src = fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "double-conversion";
      rev = "0f9ddde468687d25fa6c4c9accb02a034bc2f9c3";
      sha256 = "0sjljf1sbwalw1zycpjf6bqhljag9i1k77b18b0fd1pzrc29wnks";
    };
  });
  say = overrideCabal super.say (drv: {
    patches = (drv.patches or []) ++ [
      ./say.patch
    ];
    buildDepends = (drv.buildDepends or []) ++ [
      self.ghcjs-base
    ];
  });
  aeson = appendPatch super.aeson ./aeson.patch;
}
