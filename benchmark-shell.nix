{ reflexPlatform ? import ./. {}
}:
with reflexPlatform.nixpkgs;
reflexPlatform.pinBuildInputs "benchmark-shell" [
  nodejs-8_x
  nodePackages.npm
  chromium
  chromedriver
] []
