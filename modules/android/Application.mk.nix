{ pkgs
, abiVersions
}:
''
APP_ABI := ${pkgs.lib.concatStringsSep " " abiVersions}
''
