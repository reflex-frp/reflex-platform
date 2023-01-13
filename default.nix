import ./project.nix rec {
  name = "reflex-todomvc";
  src = ../reflex-todomvc;
  compiler-nix-name = "ghc8107Splices";
  extraSrcFiles = {
    library.extraSrcFiles = [ "style.css" ];
    exes.reflex-todomvc.extraSrcFiles = [ "style.css" ];
  };
  overrides = [
    #{ packages.reflex.configureFlags = [ "-f-use-template-haskell" ]; }
    ({ config, lib, pkgs, ... }: { packages.${name}.components = {
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
    })
    ({config, pkgs, lib, ... }: { 
	packages.jsaddle-wkwebview.components.library = {
	configureFlags = pkgs.lib.optionals (pkgs.stdenv.targetPlatform.isiOS) [ "-f-include-app-delegate" ];
		frameworks = if (pkgs.stdenv.targetPlatform.isiOS) then lib.mkForce [ pkgs.darwin.iosSdkPkgs.sdk ]
		#[ pkgs.darwin.apple_sdk.frameworks.AppKit pkgs.darwin.apple_sdk.frameworks.CoreFoundation pkgs.darwin.apple_sdk.frameworks.UserNotifications ] 
	else [];
	};
     })
    { packages.reflex-todomvc.src = src; }
    ({ config, lib, ... }: { packages.bitvec.patches = lib.mkForce [ ]; })
  ];
}
