this:
{ config, options, lib }: let
  pkgs = this.nixpkgs;
  inherit (lib) mapAttrs mapAttrsToList escapeShellArg
    optionalString concatStringsSep concatMapStringsSep;
  inherit (config) packages shells overrides tools
    withHoogle android ios;
  preparePackageConfig =
    name: appConfig:
      builtins.removeAttrs appConfig ["_module"]
      // lib.optionalAttrs (appConfig.package == null) { package = p: p.${name}; };
  overrides' = lib.composeExtensions
    (self: super: mapAttrs (name: path: self.callCabal2nix name path {}) packages) overrides;
  mkPkgSet = name: _: this.${name}.override { overrides = overrides'; };
  prj = mapAttrs mkPkgSet shells // {
    shells = mapAttrs (name: pnames:
      this.workOnMulti' {
        env = prj.${name}.override { overrides = self: super: lib.optionalAttrs withHoogle {
          ghcWithPackages = self.ghcWithHoogle;
        }; };
        packageNames = pnames;
        inherit tools;
      }
    ) shells;

    android =
      mapAttrs (name: appConfig:
        let
          ghcAndroidArm64 = this.ghcAndroidArm64.override { overrides = overrides'; };
          ghcAndroidArmv7a = this.ghcAndroidArmv7a.override { overrides = overrides'; };
        in (this.androidWithHaskellPackages { inherit ghcAndroidArm64 ghcAndroidArmv7a; }).buildApp
          (preparePackageConfig name appConfig)
      ) android;

    ios =
      mapAttrs (name: appConfig:
        let ghcIosArm64 = this.ghcIosArm64.override { overrides = overrides'; };
        in (this.iosWithHaskellPackages ghcIosArm64).buildApp
          (preparePackageConfig name appConfig)
      ) ios;

    reflex = this;

    all = all true;

    inherit config options;
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

  all = includeRemoteBuilds:
    let tracedMobileLinks = mobileName: system: mobile:
      let
        build = mobileLinks mobileName mobile;
        msg = ''


          Skipping ${mobileName} apps; system is ${this.system}, but ${system} is needed.
          Use `nix-build -A all` to build with remote machines.
          See: https://nixos.org/nixos/manual/options.html#opt-nix.buildMachines

        '';
      in if mobile == {} then ""
        else if includeRemoteBuilds then build
          else if system != this.system then builtins.trace msg ""
            # TODO: This is a bit of a hack. `this.iosSupport` prints
            # a warning and returns false when *the local system*
            # doesn't have the SDK. Just because `includeRemoteBuilds`
            # is off doesn't mean we know this is the system iOS apps
            # will build on. Nonetheless, it's important not to
            # evaluate `this.iosSupport` if we don't need to, as it
            # may `trace` an unnecessary warning.
            else if system == "x86_64-darwin" -> this.iosSupport then build
              else "";
    in pkgs.runCommand config.name { passthru = prj; preferLocalBuild = true; } ''
      ${concatStringsSep "\n" ghcLinks}
      ${tracedMobileLinks "android" "x86_64-linux" prj.android}
      ${tracedMobileLinks "ios" "x86_64-darwin" prj.ios}
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
    project = all false;
    reflex = this;
    _module.args = { pkgs = this.nixpkgs; inherit (config) project reflex; };
  };
}
