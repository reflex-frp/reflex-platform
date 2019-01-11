this:
{ config, options, lib }: let
  pkgs = this.nixpkgs;
  inherit (lib) mapAttrs mapAttrsToList escapeShellArg
    optionalAttrs optionalString concatStringsSep concatMapStringsSep;
  inherit (config) packages shells overrides tools useWarp shellToolOverrides passthru
    withHoogle android ios;
  preparePackageConfig =
    name: appConfig:
      builtins.removeAttrs appConfig ["_module"]
      // lib.optionalAttrs (appConfig.package == null) { package = p: p.${name}; };
  overrides' = pkgs.lib.foldr pkgs.lib.composeExtensions (_: _: {}) [
    (self: super: mapAttrs (name: path: self.callCabal2nix name path {}) packages)
    (self: super: {
      reflex-dom = if useWarp && (with self.ghc.stdenv; hostPlatform == targetPlatform) && !(self.ghc.isGhcjs or false)
        then pkgs.haskell.lib.addBuildDepend (pkgs.haskell.lib.enableCabalFlag super.reflex-dom "use-warp") self.jsaddle-warp
        else super.reflex-dom;
    })
    overrides
  ];
  mkPkgSet = name: _: this.${name}.override { overrides = overrides'; };
  prj = mapAttrs mkPkgSet shells // {
    shells = mapAttrs (name: pnames:
      this.workOnMulti' {
        env = prj.${name}.override { overrides = self: super: lib.optionalAttrs withHoogle {
          ghcWithPackages = self.ghcWithHoogle;
        }; };
        packageNames = pnames;
        inherit tools shellToolOverrides;
      }
    ) shells;

    android =
      mapAttrs (name: appConfig:
        let
          ghcAndroidAarch64 = this.ghcAndroidAarch64.override { overrides = overrides'; };
          ghcAndroidAarch32 = this.ghcAndroidAarch32.override { overrides = overrides'; };
        in (this.androidWithHaskellPackages { inherit ghcAndroidAarch64 ghcAndroidAarch32; }).buildApp
          (preparePackageConfig name appConfig)
      ) (optionalAttrs this.androidSupport android);

    ios =
      mapAttrs (name: appConfig:
        let ghcIosAarch64 = this.ghcIosAarch64.override { overrides = overrides'; };
        in (this.iosWithHaskellPackages ghcIosAarch64).buildApp
          (preparePackageConfig name appConfig)
      ) (optionalAttrs this.iosSupport ios);

    reflex = this;

    inherit all passthru config options;
  };

  ghcLinks = mapAttrsToList (name: pnames: optionalString (pnames != []) ''
    mkdir -p $out/${escapeShellArg name}
    ${concatMapStringsSep "\n" (n: ''
      ln -s ${prj.${name}.${n}} $out/${escapeShellArg name}/${escapeShellArg n}
    '') pnames}
  '') shells;
  mobileLinks = mobileName: mobile: ''
    mkdir -p $out/${escapeShellArg mobileName}
    ${concatStringsSep "\n" (mapAttrsToList (name: app: ''
      ln -s ${app} $out/${escapeShellArg mobileName}/${escapeShellArg name}
    '') mobile)}
  '';

  all =
    let tracedMobileLinks = mobileName: mobile:
          optionalString (mobile != {}) (mobileLinks mobileName mobile);
    in pkgs.runCommand config.name { passthru = prj; preferLocalBuild = true; } ''
      ${concatStringsSep "\n" ghcLinks}
      ${tracedMobileLinks "android" prj.android}
      ${tracedMobileLinks "ios" prj.ios}
    '';
in {
  options = {
    reflex = lib.mkOption {
      type = lib.types.unspecified;
      internal = true;
      visible = false;
    };

    project = lib.mkOption {
      type = lib.types.package;
      internal = true;
      visible = false;
    };
  };

  config = {
    project = all;
    reflex = this;
    _module.args = { pkgs = this.nixpkgs; inherit (config) project reflex; };
  };
}
