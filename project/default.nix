this:

let
  inherit (this) nixpkgs;
  inherit (nixpkgs.lib) mapAttrs mapAttrsToList escapeShellArg
    optionalAttrs optionalString concatStringsSep concatMapStringsSep;
  workOnMulti = import ../nix-utils/work-on-multi { reflex-platform = this; };
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
#     $ nix-build
#     $ nix-build -A ghc.backend
#     $ nix-build -A ghcjs.frontend
#     $ nix-build -A android.frontend
#
#     $ nix-shell -A shells.ghc
#     $ nix-shell -A shells.ghcjs
#
{ name ? "reflex-project"
  # An optional name for your entire project.

, packages
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
  #     };

, shellToolOverrides ? _: _: {}
  # A function returning a record of tools to provide in the
  # nix-shells.
  #
  #     shellToolOverrides = ghc: super: {
  #       inherit (ghc) hpack;
  #       inherit (pkgs) chromium;
  #       ghc-mod = null;
  #       cabal-install = ghc.callHackage "cabal-install" "2.0.0.1" {};
  #       ghcid = pkgs.haskell.lib.justStaticExecutables super.ghcid;
  #     };
  #
  # Some tools, like `ghc-mod`, have to be built with the same GHC as
  # your project. The argument to the `tools` function is the haskell
  # package set of the platform we are developing for, allowing you to
  # build tools with the correct Haskell package set.
  #
  # The second argument, `super`, is the record of tools provided by
  # default. You can override these defaults by returning values with
  # the same name in your record. They can be disabled by setting them
  # to null.

, tools ? _: []
  # An older, obsolete version of `shellToolOverrides`.
  #
  #     tools = ghc: with ghc; [ hpack pkgs.chromium ];

, withHoogle ? true
  # Set to false to disable building the hoogle database when entering
  # the nix-shell.

, useWarp ? false
  # Configure `reflex-dom` to use `jsaddle-warp`.

, android ? {}
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

, ios ? {}
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

, passthru ? {}

}:
let
  overrides' = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions (_: _: {}) [
    (self: super: mapAttrs (name: path: self.callCabal2nix name path {}) packages)
    (self: super: {
      reflex-dom = if useWarp && (with self.ghc.stdenv; hostPlatform == targetPlatform) && !(self.ghc.isGhcjs or false)
        then nixpkgs.haskell.lib.addBuildDepend (nixpkgs.haskell.lib.enableCabalFlag super.reflex-dom "use-warp") self.jsaddle-warp
        else super.reflex-dom;
    })
    overrides
  ];
  mkPkgSet = name: _: this.${name}.override { overrides = overrides'; };
  prj = mapAttrs mkPkgSet shells // {
    shells = mapAttrs (name: pnames:
      workOnMulti {
        envFunc = _: prj.${name}.override { overrides = self: super: nixpkgs.lib.optionalAttrs withHoogle {
          ghcWithPackages = self.ghcWithHoogle;
        }; };
        packageNames = pnames;
        inherit tools shellToolOverrides;
      }
    ) shells;

    android = if this.androidSupport
      then mapAttrs (name: config:
             let
               ghcAndroidAarch64 = this.ghcAndroidAarch64.override { overrides = overrides'; };
               ghcAndroidAarch32 = this.ghcAndroidAarch32.override { overrides = overrides'; };
             in (this.androidWithHaskellPackages { inherit ghcAndroidAarch64 ghcAndroidAarch32; }).buildApp
               ({ package = p: p.${name}; } // config)
           ) android
      else throw "Android builds are not supported on this platform.";

    ios = if this.iosSupport
      then mapAttrs (name: config:
             let ghcIosAarch64 = this.ghcIosAarch64.override { overrides = overrides'; };
             in (this.iosWithHaskellPackages ghcIosAarch64).buildApp
               ({ package = p: p.${name}; } // config)
           ) ios
      else throw "iOS builds are not supported on this platform.";

    reflex = this;

    inherit passthru;
  };
in prj
