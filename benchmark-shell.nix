{ reflexPlatform ? import ./. {}
}:
with reflexPlatform.nixpkgs;

let browserTools = if system == "x86_64-darwin" then [] else [
  chromium
  chromedriver
];
in
  reflexPlatform.pinBuildInputs "benchmark-shell" ([
    nodejs-8_x
    nodePackages.npm
  ] ++ browserTools) []
