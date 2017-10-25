this:

let
  inherit (this) nixpkgs;
  inherit (nixpkgs.lib) makeExtensible mapAttrs;
in

# This function simplifies the definition of Haskell projects that
# have multiple packages. It provides shells for incrementally working
# on all your packages at once using `cabal.project` files, using any
# version of GHC provided by `reflex-platform`, including GHCJS. It
# also produces individual derivations for each package, which can
# ease devops or integration with other Nix setups.
#
# Example:
#
# > default.nix
#
#     (import ./reflex-platform {}).project ({ pkgs, ... }: {
#       packages = {
#         common = ./common;
#         backend = ./backend;
#         frontend = ./frontend;
#       };
#
#       shells = {
#         ghc = ["common" "backend" "frontend"];
#         ghcjs = ["common" "frontend"];
#       };
#
#       android.frontend = {
#         executableName = "frontend";
#         applicationId = "org.example.frontend";
#         displayName = "Example App";
#       };
#     })
#
# > example commands
#
#     $ nix-build -A ghc.backend
#     $ nix-build -A ghcjs.frontend
#     $ nix-build -A android.frontend
#     $ nix-shell -A shells.ghc
#     $ nix-shell -A shells.ghcjs
#
{ packages
  # :: { <package name> :: Path }
  #
  # An attribute set of local packages being developed. Keys are the
  # cabal package name and values are the path to the source
  # directory.

, shells ? {}
  # :: { <platform name> :: [PackageName] }
  #
  # The `shells` field defines which platforms we'd like to develop
  # for, and which packages' dependencies we want available in the
  # development sandbox for that platform. Note in the example above
  # that specifying `common` is important; otherwise it will be
  # treated as a dependency that needs to be built by Nix for the
  # sandbox. You can use these shells with `cabal.project` files to
  # build all three packages in a shared incremental environment, for
  # both GHC and GHCJS.

, overrides ? _: _: {}
  # :: PackageSet -> PackageSet ->  { <package name> :: Derivation }
  #
  # A function for overriding Haskell packages. You can use
  # `callHackage` and `callCabal2nix` to bump package versions or
  # build them from GitHub. e.g.
  #
  #     overrides = self: super: {
  #       lens = self.callHackage "lens" "4.15.4" {};
  #       free = self.callCabal2nix "free" (pkgs.fetchFromGitHub {
  #         owner = "ekmett";
  #         repo = "free";
  #         rev = "a0c5bef18b9609377f20ac6a153a20b7b94578c9";
  #         sha256 = "0vh3hj5rj98d448l647jc6b6q1km4nd4k01s9rajgkc2igigfp6s";
  #       }) {};
  #     }

, android ? throw "No Android config"
  # ::
  # { <app name> ::
  #   { executableName :: String
  #   , applicationId :: String
  #   , displayName :: String
  #   , package :: PackageSet -> Derivation
  #     ^ Optional
  #   }
  # }
  #
  # Use this argument to configure android apps. The returned
  # derivations will be in `android.<app name>`. The `package`
  # argument can be set to use a different Haskell package than the
  # one named <app name>.

, ios ? throw "No iOS config"
  # ::
  # { <app name> ::
  #   { executableName :: String
  #   , bundleIdentifier :: String
  #   , bundleName :: String
  #   , package :: PackageSet -> Derivation
  #     ^ Optional
  #   }
  # }
  #
  # Use this argument to configure iOS apps. The returned derivations
  # will be in `ios.<app name>`. The `package` argument can be set to
  # use a different Haskell package than the one named <app name>.

}:
let
  overrides' = nixpkgs.lib.composeExtensions overrides
    (self: super: mapAttrs (name: path: self.callCabal2nix name path {}) packages);
  mkPkgSet = name: _: this.${name}.override { overrides = overrides'; };
in makeExtensible (prj: mapAttrs mkPkgSet shells // {
  shells = mapAttrs (name: pnames:
    this.workOnMulti (prj.${name}.override { overrides = self: super: {
      ghcWithPackages = self.ghcWithHoogle;
    }; }) pnames) shells;

  android = mapAttrs (name: config:
    let
      ghcAndroidArm64 = this.ghcAndroidArm64.override { overrides = overrides'; };
      ghcAndroidArmv7a = this.ghcAndroidArmv7a.override { overrides = overrides'; };
    in (this.androidWithHaskellPackages { inherit ghcAndroidArm64 ghcAndroidArmv7a; }).buildApp
      ({ package = p: p.${name}; } // config)
  ) android;

  ios = mapAttrs (name: config:
    let ghcIosArm64 = this.ghcIosArm64.override { overrides = overrides'; };
    in (this.iosWithHaskellPackages ghcIosArm64).buildApp
      ({ package = p: p.${name}; } // config)
  ) ios;

  reflex = this;
})
