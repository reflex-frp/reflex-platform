{ nixpkgs
, abiVersions
}:
''
APP_ABI := ${nixpkgs.lib.concatStringsSep " " abiVersions}
''
