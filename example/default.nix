# Example usage of this project
let
  nix-thunk = import ../submodules/nix-thunk { };
in
import ../project.nix rec {
  name = "reflex-todomvc";
  patchNixpkgs = true;
  remotePatches = [
    {
      url = "https://github.com/obsidiansystems/nixpkgs/commit/d39ee6b7c45deb224d95f717bd1e6e2144e09dd9.diff";
      sha256 = "sha256-stn4C43O5M0Qk80gj7YK/87qCDflnm/AwYcOXv5fErI=";
    }
    {
      url = "https://github.com/obsidiansystems/nixpkgs/commit/4516c1a5bb5d11209324bd00239448528bd5fb6d.diff";
      sha256 = "sha256-6GyCvZbuquVS++xR68e+jb4IiFPlIbbJb/kmc9uTers=";
    }
  ];
  src = nix-thunk.thunkSource ../submodules/reflex-todomvc;
  compiler-nix-name = "ghc8107Splices";
  android_sdk_accept_license = true;
  allowUnfree = true;
  extraSrcFiles = {
    library.extraSrcFiles = [ "style.css" ];
    exes.reflex-todomvc.extraSrcFiles = [ "style.css" ];
  };
  hackageOverlays = pkgs: [
    #{
    #  name = "android-activity";
    #  version = "0.1.1";
    #  src = pkgs.fetchFromGitHub {
    #    owner = "obsidiansystems";
    #    repo = "android-activity";
    #    rev = "2bc40f6f907b27c66428284ee435b86cad38cff8";
    #    sha256 = "sha256-AIpbe0JZX68lsQB9mpvR7xAIct/vwQAARVHAK0iChV4=";
    #  };
    #}
  ];
  overrides = [
    ({ config, pkgs, lib, ... }: {
      packages.reflex-dom = {
	    flags = {
	      webkit2gtk = if (pkgs.stdenv.targetPlatform.isAndroid) then lib.mkForce false else true;
	    };
      };
      packages.jsaddle-wkwebview.components.library = {
        configureFlags = pkgs.lib.optionals (pkgs.stdenv.targetPlatform.isiOS) [ "-f-include-app-delegate" ];
        frameworks =
          if (pkgs.stdenv.targetPlatform.isiOS) then lib.mkForce [ pkgs.darwin.iosSdkPkgs.sdk ]
          else [ ];
      };
    })
    #({ config, lib, ... }: { packages.reflex-todomvc.src = lib.mkForce src; })
    ({ config, lib, ... }: { packages.bitvec.patches = lib.mkForce [ ]; })
  ];
}
