# NOTE: Define the interface
{ nixpkgs ? ./dep/nixpkgs
, patches ? [ ]
, doPatch ? false
, nixpkgsOverlays ? (_: _: { })
, android_sdk_accept_license ? false
, allowUnfree ? false
, haskellNixArgs ? { }
, system ? builtins.currentSystem
}:
let
  composeExtensions =
    f: g: final: prev:
    let
      fApplied = f final prev;
      prev' = prev // fApplied;
    in
    fApplied // g final prev';

  # TODO:
  # - Remove this let box properly
  # Logic to bootstrap packages that isn't our local checkout

  # Auto add deps for everything in ./dep
  deps = rec {
    imported = {
      nix-thunk = import ./dep/nix-thunk { };
      haskell-nix = import ./dep/haskell.nix (haskellNixArgs // { pkgs = bootPkgs; });
    };

    source = imported.nix-thunk.mapSubdirectories imported.nix-thunk.thunkSource ./dep;
  };

  # Setup our special overlays and config
  obsidian = {
    overlays = deps.imported.haskell-nix.nixpkgsArgs.overlays ++ [
      nixpkgsOverlays
      (self: super: {
        binutils-unwrapped = super.binutils-unwrapped.override {
          autoreconfHook = super.lib.optional self.stdenv.buildPlatform.isDarwin super.autoreconfHook269;
        };
        darwin = super.darwin // {
          libiconv = super.darwin.libiconv.overrideAttrs (_:
            super.lib.optionalAttrs (self.stdenv.hostPlatform != self.stdenv.buildPlatform) {
              postInstall = "rm $out/include/libcharset.h $out/include/localcharset.h";
              configureFlags = ["--disable-shared" "--enable-static"];
            });
          };

        zlib = super.zlib.override (super.lib.optionalAttrs
          (self.stdenv.hostPlatform != self.stdenv.buildPlatform)
          { static = true; shared = false; });
      })
      (import ./modules/overlays/default.nix {
        inherit deps;
        inherit composeExtensions;
      }).combined
    ];
    config = deps.imported.haskell-nix.nixpkgsArgs.config // {
      android_sdk.accept_license = android_sdk_accept_license;
      inherit allowUnfree;
    };
  };

  bootPkgs = import nixpkgs {
    inherit system;
  };
  # Setup bootstrap pkgs, or alternatively the main packages
  pkgs-pre = import nixpkgs {
    inherit (obsidian) overlays config;
    inherit system;
  };

  # Patch the packages with some commits external to our specific checkout
  # this is optional, if people feel the need to use their own nixpkgs
  patchedNixpkgs = (pkgs-pre.applyPatches {
    name = "patched-nixpkgs";
    src = (import nixpkgs {}).path;
    patches = map pkgs-pre.fetchpatch patches;
  });

  patched-pkgs = import patchedNixpkgs ({
    inherit (obsidian) overlays config;
    inherit system;
  });

  # Our final packages with the patched commits
  pkgs = if doPatch then patched-pkgs else pkgs-pre;
in
def:
let
  toplevel = {
    inherit pkgs;
    inherit (deps.imported.nix-thunk) thunkSource mapSubdirectories;
  };
  proj = import ./modules/project.nix { inherit pkgs deps obsidian; };
in
proj (def toplevel)
