# Example usage of this project
{
  project ? import ../default.nix {
    android_sdk_accept_license = true;
    allowUnfree = true;
  }
}:
project ({ pkgs, thunkSource, ... }: {
  name = "reflex-todomvc";
  plugins = [
    (self: super: {
      __passthru = null;
      test-plugin = {
        inherit (self) pkgs hsPkgs;
      };
    })
    (self: super: {
      multiple-plugin-test = "hello";
    })
  ];
  src = thunkSource ../dep/reflex-todomvc;
  compiler-nix-name = "ghc8107Splices";
  ghcjs-compiler-nix-name = "ghcjs8107";
  extraSrcFiles = {
    library.extraSrcFiles = [ "style.css" ];
    exes.reflex-todomvc.extraSrcFiles = [ "style.css" ];
  };
  hackageOverlays = [
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
})
