import ./project.nix rec {
  name = "reflex-todomvc";
  src = ../reflex-todomvc;
  compiler-nix-name = "ghc8107Splices";
  android_sdk_accept_license = true;
  allowUnfree = true;
  extraSrcFiles = {
    library.extraSrcFiles = [ "style.css" ];
    exes.reflex-todomvc.extraSrcFiles = [ "style.css" ];
  };
  hackageOverlays = pkgs: [
    { 
      name = "android-activity"; 
      version = "0.1.1"; 
      src = pkgs.fetchFromGitHub {
        owner = "obsidiansystems";
        repo = "android-activity";
        rev = "2bc40f6f907b27c66428284ee435b86cad38cff8";
        sha256 = "sha256-AIpbe0JZX68lsQB9mpvR7xAIct/vwQAARVHAK0iChV4=";
	#sha256 = "sha256-CveWAOQDOjhX8hmRzCLImOkaabTytgftM6I7Sya68bM=";
      };
      #src = (builtins.fetchGit { url = "https://github.com/obsidiansystems/android-activity.git"; rev = "a51bf130b04af92645c040df065c54161e99a335"; 
    }
  ];
  overrides = [
    #{ packages.reflex.configureFlags = [ "-f-use-template-haskell" ]; }
    ({ config, lib, pkgs, ... }: {
 /*     packages.${name}.components = pkgs.lib.optionalAttrs (pkgs.stdenv.targetPlatform.isiOS) {
        library = {
          depends = [
            config.hsPkgs.jsaddle-wkwebview
          ];
          frameworks = [
            pkgs.darwin.apple_sdk.frameworks.CoreFoundation
            pkgs.darwin.apple_sdk.frameworks.Foundation
            pkgs.darwin.apple_sdk.frameworks.AppKit
            #pkgs.darwin.apple_sdk.frameworks.UserNotifications
          ];
        };
        exes.reflex-todomvc.frameworks = [
          #pkgs.darwin.apple_sdk.frameworks.AppKit
          #pkgs.darwin.apple_sdk.frameworks.WebKit
          #pkgs.darwin.apple_sdk.frameworks.Foundation
          #pkgs.darwin.apple_sdk.frameworks.CoreFoundation
          #pkgs.darwin.apple_sdk.frameworks.Cocoa
        ];
      };
  */
    })
    ({ config, pkgs, lib, ... }: {
      packages.android-activity.components.library = lib.optionalAttrs (pkgs.stdenv.targetPlatform.isAndroid) {
        depends = [
	  pkgs.buildPackages.buildPackages.jdk
	  pkgs.androidndkPkgs_23b.libraries.headers
        ];
        cSources = [
          pkgs.androidndkPkgs_23b.libraries.headers
        ];
	configureFlags = [ 
	  "--extra-lib-dirs=${pkgs.androidndkPkgs_23b.libraries.headers}"
	  "--extra-include-dirs=${pkgs.androidndkPkgs_23b.libraries.headers}"
	];
      };
      packages.reflex-dom = {
	flags = {
	  webkit2gtk = lib.mkForce false;
	};
      };
      packages.jsaddle-wkwebview.components.library = {
        configureFlags = pkgs.lib.optionals (pkgs.stdenv.targetPlatform.isiOS) [ "-f-include-app-delegate" ];
        frameworks =
          if (pkgs.stdenv.targetPlatform.isiOS) then lib.mkForce [ pkgs.darwin.iosSdkPkgs.sdk ]
          #[ pkgs.darwin.apple_sdk.frameworks.AppKit pkgs.darwin.apple_sdk.frameworks.CoreFoundation pkgs.darwin.apple_sdk.frameworks.UserNotifications ] 
          else [ ];
      };
    })
    #({ config, lib, ... }: { packages.reflex-todomvc.src = lib.mkForce src; })
    ({ config, lib, ... }: { packages.bitvec.patches = lib.mkForce [ ]; })
  ];
}
