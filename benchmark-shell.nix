with (import ./. {}).nixpkgs; runCommand "shell" { buildInputs = [ nodejs-8_x nodePackages.npm ]; } ""
